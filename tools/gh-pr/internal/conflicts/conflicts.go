package conflicts

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/vdemeester/home/tools/gh-pr/internal/output"
)

// Resolver handles merge conflict resolution
type Resolver struct {
	out         *output.Writer
	worktreeDir string
	useWorktree bool
	autoPush    bool
}

// NewResolver creates a new conflict resolver
func NewResolver(out *output.Writer, worktreeDir string, useWorktree, autoPush bool) *Resolver {
	return &Resolver{
		out:         out,
		worktreeDir: worktreeDir,
		useWorktree: useWorktree,
		autoPush:    autoPush,
	}
}

// PRInfo contains information about a pull request
type PRInfo struct {
	Number            int    `json:"number"`
	Title             string `json:"title"`
	HeadRefName       string `json:"headRefName"`
	BaseRefName       string `json:"baseRefName"`
	Mergeable         string `json:"mergeable"`
	URL               string `json:"url"`
	IsCrossRepository bool   `json:"isCrossRepository"`
	HeadRepository    struct {
		Name          string `json:"name"`
		NameWithOwner string `json:"nameWithOwner"`
	} `json:"headRepository"`
	HeadRepositoryOwner struct {
		Login string `json:"login"`
	} `json:"headRepositoryOwner"`
	Author struct {
		Login string `json:"login"`
	} `json:"author"`
}

// FindConflictingPR finds a specific PR and checks if it has conflicts
func (r *Resolver) FindConflictingPR(repo, prNumber string) (*PRInfo, error) {
	r.out.Info("Fetching PR #%s from %s...", prNumber, repo)

	args := []string{"pr", "view", prNumber, "-R", repo,
		"--json", "number,title,headRefName,baseRefName,author,mergeable,url,isCrossRepository,headRepository,headRepositoryOwner"}

	cmd := exec.Command("gh", args...)
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to fetch PR: %w", err)
	}

	var pr PRInfo
	if err := json.Unmarshal(output, &pr); err != nil {
		return nil, fmt.Errorf("failed to parse PR info: %w", err)
	}

	if pr.Mergeable != "CONFLICTING" {
		return nil, fmt.Errorf("PR #%s does not have merge conflicts (status: %s)", prNumber, pr.Mergeable)
	}

	return &pr, nil
}

// FindConflictingPRs searches for all conflicting PRs for a given author/org
func (r *Resolver) FindConflictingPRs(org, author string) ([]PRInfo, error) {
	r.out.Info("Searching for conflicting pull requests...")

	// Build search query
	args := []string{"search", "prs", "--author", author, "--state", "open"}
	if org != "" {
		args = append(args, "--owner", org)
	}
	args = append(args, "--json", "number,title,repository,url", "--limit", "100")

	cmd := exec.Command("gh", args...)
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to search PRs: %w", err)
	}

	var searchResults []struct {
		Number     int    `json:"number"`
		Title      string `json:"title"`
		URL        string `json:"url"`
		Repository struct {
			NameWithOwner string `json:"nameWithOwner"`
		} `json:"repository"`
	}

	if err := json.Unmarshal(output, &searchResults); err != nil {
		return nil, fmt.Errorf("failed to parse search results: %w", err)
	}

	if len(searchResults) == 0 {
		return nil, nil
	}

	r.out.Info("Checking %d PRs for merge conflicts...", len(searchResults))

	var conflictingPRs []PRInfo
	for i, result := range searchResults {
		r.out.Print("\rChecking PR %d/%d...", i+1, len(searchResults))

		// Fetch detailed PR info
		pr, err := r.FindConflictingPR(result.Repository.NameWithOwner, fmt.Sprintf("%d", result.Number))
		if err != nil {
			// Skip non-conflicting PRs
			continue
		}

		conflictingPRs = append(conflictingPRs, *pr)
	}

	fmt.Println() // Newline after progress
	return conflictingPRs, nil
}

// ResolvePR resolves conflicts for a single PR
func (r *Resolver) ResolvePR(repo string, pr *PRInfo) error {
	r.out.Info("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
	r.out.Info("Repository: %s", repo)
	r.out.Info("PR #%d: %s", pr.Number, pr.Title)
	r.out.Info("Branch: %s -> %s", pr.HeadRefName, pr.BaseRefName)
	r.out.Info("URL: %s", pr.URL)
	r.out.Info("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
	r.out.Println("")

	// Determine fork repository
	var forkRepo string
	if pr.IsCrossRepository {
		forkRepo = pr.HeadRepository.NameWithOwner
		if forkRepo == "" {
			forkRepo = fmt.Sprintf("%s/%s", pr.HeadRepositoryOwner.Login, pr.HeadRepository.Name)
		}
		r.out.Info("PR is from fork: %s", forkRepo)
	} else {
		forkRepo = repo
		r.out.Info("PR is from branch in same repo")
	}

	var workDir string
	var err error

	if r.useWorktree {
		workDir, err = r.setupWorktree(repo, forkRepo, pr)
		if err != nil {
			return err
		}
	} else {
		workDir, err = r.setupExistingRepo(repo, forkRepo, pr)
		if err != nil {
			return err
		}
	}

	// Change to work directory
	if err := os.Chdir(workDir); err != nil {
		return fmt.Errorf("failed to change to work directory: %w", err)
	}

	// Perform rebase
	if err := r.performRebase(repo, pr, forkRepo); err != nil {
		return err
	}

	// Push changes
	if r.autoPush {
		r.out.Info("Force-pushing changes...")
		cmd := exec.Command("git", "push", "--force-with-lease")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			r.out.Error("Failed to push changes")
			r.out.Warning("You may need to push manually from: %s", workDir)
			return err
		}
		r.out.Success("✓ Changes pushed successfully!")
	} else {
		r.out.Warning("Changes not pushed. To push manually:")
		r.out.Println("  cd %s", workDir)
		r.out.Println("  git push --force-with-lease")
	}

	r.out.Println("")

	if r.useWorktree {
		r.out.Warning("Note: Worktree kept at: %s", workDir)
		r.out.Warning("To remove: git worktree remove %s", workDir)
	}

	return nil
}

func (r *Resolver) setupWorktree(repo, forkRepo string, pr *PRInfo) (string, error) {
	repoName := strings.ReplaceAll(repo, "/", "-")
	workDir := filepath.Join(r.worktreeDir, repoName, fmt.Sprintf("pr-%d", pr.Number))

	r.out.Info("Creating worktree at: %s", workDir)

	// Create parent directory
	if err := os.MkdirAll(filepath.Join(r.worktreeDir, repoName), 0755); err != nil {
		return "", fmt.Errorf("failed to create worktree directory: %w", err)
	}

	// Clone/fetch repository
	repoDir := filepath.Join(r.worktreeDir, repoName, "main")
	if _, err := os.Stat(repoDir); os.IsNotExist(err) {
		r.out.Info("Cloning fork: %s...", forkRepo)
		cmd := exec.Command("gh", "repo", "clone", forkRepo, repoDir, "--", "--bare")
		if err := cmd.Run(); err != nil {
			return "", fmt.Errorf("failed to clone repository: %w", err)
		}

		// Add upstream remote if this is a fork
		if pr.IsCrossRepository {
			r.out.Info("Adding upstream remote: %s...", repo)
			cmd := exec.Command("git", "-C", repoDir, "remote", "add", "upstream", fmt.Sprintf("https://github.com/%s.git", repo))
			cmd.Run() // Ignore error if already exists
		}
	} else {
		r.out.Info("Fetching latest changes from fork...")
		cmd := exec.Command("git", "-C", repoDir, "fetch", "origin")
		if err := cmd.Run(); err != nil {
			return "", fmt.Errorf("failed to fetch: %w", err)
		}

		// Ensure upstream exists
		if pr.IsCrossRepository {
			cmd := exec.Command("git", "-C", repoDir, "remote", "get-url", "upstream")
			if err := cmd.Run(); err != nil {
				r.out.Info("Adding upstream remote: %s...", repo)
				cmd := exec.Command("git", "-C", repoDir, "remote", "add", "upstream", fmt.Sprintf("https://github.com/%s.git", repo))
				cmd.Run()
			}
		}
	}

	// Fetch from upstream if fork
	if pr.IsCrossRepository {
		r.out.Info("Fetching from upstream: %s...", repo)
		cmd := exec.Command("git", "-C", repoDir, "fetch", "upstream")
		if err := cmd.Run(); err != nil {
			return "", fmt.Errorf("failed to fetch upstream: %w", err)
		}
	}

	// Remove existing worktree if present
	if _, err := os.Stat(workDir); err == nil {
		r.out.Info("Removing existing worktree...")
		cmd := exec.Command("git", "-C", repoDir, "worktree", "remove", workDir, "--force")
		cmd.Run() // Ignore error
		os.RemoveAll(workDir)
	}

	// Fetch PR branch
	r.out.Info("Fetching PR branch: %s...", pr.HeadRefName)
	cmd := exec.Command("git", "-C", repoDir, "fetch", "origin", fmt.Sprintf("%s:pr-%d", pr.HeadRefName, pr.Number))
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("failed to fetch PR branch: %w", err)
	}

	// Create worktree
	r.out.Info("Creating worktree for branch %s...", pr.HeadRefName)
	cmd = exec.Command("git", "-C", repoDir, "worktree", "add", workDir, fmt.Sprintf("pr-%d", pr.Number))
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("failed to create worktree: %w", err)
	}

	return workDir, nil
}

func (r *Resolver) setupExistingRepo(repo, forkRepo string, pr *PRInfo) (string, error) {
	r.out.Info("Using existing repository (no worktree)")

	cwd, err := os.Getwd()
	if err != nil {
		return "", err
	}

	// Verify we're in the correct repository
	cmd := exec.Command("gh", "repo", "view", "--json", "nameWithOwner", "-q", ".nameWithOwner")
	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to get current repo: %w", err)
	}

	currentRepo := strings.TrimSpace(string(output))
	if currentRepo != forkRepo {
		return "", fmt.Errorf("current directory is %s, expected %s. Please cd to your fork or use --worktree mode", currentRepo, forkRepo)
	}

	// Ensure upstream remote exists if fork
	if pr.IsCrossRepository {
		cmd := exec.Command("git", "remote", "get-url", "upstream")
		if err := cmd.Run(); err != nil {
			r.out.Info("Adding upstream remote: %s...", repo)
			cmd := exec.Command("git", "remote", "add", "upstream", fmt.Sprintf("https://github.com/%s.git", repo))
			if err := cmd.Run(); err != nil {
				return "", fmt.Errorf("failed to add upstream: %w", err)
			}
		}

		r.out.Info("Fetching from upstream...")
		cmd = exec.Command("git", "fetch", "upstream")
		if err := cmd.Run(); err != nil {
			return "", fmt.Errorf("failed to fetch upstream: %w", err)
		}
	}

	// Checkout PR
	r.out.Info("Checking out PR #%d...", pr.Number)
	cmd = exec.Command("gh", "pr", "checkout", fmt.Sprintf("%d", pr.Number), "-R", repo)
	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("failed to checkout PR: %w", err)
	}

	return cwd, nil
}

func (r *Resolver) performRebase(repo string, pr *PRInfo, forkRepo string) error {
	// Determine base remote
	baseRemote := "origin"
	if pr.IsCrossRepository {
		baseRemote = "upstream"
	}

	// Fetch base branch
	r.out.Info("Fetching base branch from %s: %s", baseRemote, pr.BaseRefName)
	cmd := exec.Command("git", "fetch", baseRemote, pr.BaseRefName)
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("failed to fetch base branch: %w", err)
	}

	// Start rebase
	r.out.Info("Starting rebase onto %s/%s...", baseRemote, pr.BaseRefName)
	r.out.Println("")

	cmd = exec.Command("git", "rebase", fmt.Sprintf("%s/%s", baseRemote, pr.BaseRefName))
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Run(); err == nil {
		r.out.Success("✓ Rebase completed successfully with no conflicts!")
		return nil
	}

	// Conflicts detected
	r.out.Warning("Conflicts detected. Starting conflict resolution...")
	r.out.Println("")

	// Get conflicted files
	cmd = exec.Command("git", "diff", "--name-only", "--diff-filter=U")
	output, err := cmd.Output()
	if err != nil {
		r.out.Error("Failed to get conflicted files")
		exec.Command("git", "rebase", "--abort").Run()
		return fmt.Errorf("failed to get conflicted files: %w", err)
	}

	conflictedFiles := strings.Split(strings.TrimSpace(string(output)), "\n")
	if len(conflictedFiles) == 0 || conflictedFiles[0] == "" {
		r.out.Error("Rebase failed but no conflicted files found")
		exec.Command("git", "rebase", "--abort").Run()
		return fmt.Errorf("rebase failed with no conflicts")
	}

	r.out.Info("Conflicted files:")
	for _, file := range conflictedFiles {
		r.out.Error("  ✗ %s", file)
	}
	r.out.Println("")

	// Resolve conflicts
	for _, file := range conflictedFiles {
		if err := r.resolveConflict(file); err != nil {
			r.out.Error("Failed to resolve conflict in %s: %v", file, err)
			exec.Command("git", "rebase", "--abort").Run()
			return fmt.Errorf("conflict resolution failed: %w", err)
		}
	}

	// Continue rebase
	r.out.Info("Continuing rebase...")
	cmd = exec.Command("git", "rebase", "--continue")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		r.out.Error("Failed to continue rebase")
		r.out.Warning("You may need to resolve remaining conflicts manually")
		return fmt.Errorf("failed to continue rebase: %w", err)
	}

	r.out.Println("")
	r.out.Success("✓ Conflicts resolved successfully!")
	r.out.Println("")

	return nil
}

func (r *Resolver) resolveConflict(file string) error {
	r.out.Info("Resolving: %s", file)

	// Check if emacs is available
	if _, err := exec.LookPath("emacs"); err == nil {
		// Use emacs ediff
		r.out.Success("Launching emacs ediff for: %s", file)

		// Use git mergetool with emacs
		cmd := exec.Command("git", "mergetool", "--tool=emerge", file)
		cmd.Stdin = os.Stdin
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr

		if err := cmd.Run(); err != nil {
			r.out.Warning("Emacs ediff failed, falling back to default mergetool")
		} else {
			// Check if resolved
			if !r.hasConflictMarkers(file) {
				cmd := exec.Command("git", "add", file)
				cmd.Run()
				return nil
			}
		}
	}

	// Fallback to default git mergetool
	r.out.Info("Using git mergetool...")
	cmd := exec.Command("git", "mergetool", file)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Run(); err != nil {
		return err
	}

	// Verify resolution
	if r.hasConflictMarkers(file) {
		return fmt.Errorf("conflict markers still present in %s", file)
	}

	// Stage file
	cmd = exec.Command("git", "add", file)
	return cmd.Run()
}

func (r *Resolver) hasConflictMarkers(file string) bool {
	content, err := os.ReadFile(file)
	if err != nil {
		return false
	}
	return strings.Contains(string(content), "<<<<<<<")
}
