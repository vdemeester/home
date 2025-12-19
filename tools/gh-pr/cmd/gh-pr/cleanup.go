package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/spf13/cobra"
	"github.com/vdemeester/home/tools/gh-pr/internal/output"
)

func cleanupCmd(out *output.Writer) *cobra.Command {
	var (
		worktreeDir    string
		dryRun         bool
		checkMerged    bool
		checkUpstream  bool
		force          bool
		upstreamBranch string
	)

	cmd := &cobra.Command{
		Use:   "cleanup [DIRECTORY]",
		Short: "Clean up git worktrees without uncommitted changes",
		Long: `Remove git worktrees that have no uncommitted changes.

This command scans for git repositories with worktrees and removes those that:
  - Have no uncommitted changes (clean working directory)
  - Optionally: are merged upstream (with --check-upstream)
  - Optionally: have merged/closed PRs (with --check-merged)

By default, it scans the directory used by 'resolve-conflicts' command.

Examples:
  # Clean up default worktree directory (dry run)
  gh-pr cleanup --dry-run

  # Clean up and remove worktrees
  gh-pr cleanup

  # Clean up custom directory
  gh-pr cleanup ~/my-worktrees

  # Check if commits are merged upstream before removing
  gh-pr cleanup --check-upstream

  # Check if PR is merged before removing
  gh-pr cleanup --check-merged

  # Check both upstream and PR status
  gh-pr cleanup --check-upstream --check-merged

  # Force remove even with uncommitted changes (dangerous!)
  gh-pr cleanup --force`,
		RunE: func(cmd *cobra.Command, args []string) error {
			// Override worktreeDir if provided as argument
			if len(args) > 0 {
				worktreeDir = args[0]
			}

			return runCleanup(out, cleanupOpts{
				worktreeDir:    worktreeDir,
				dryRun:         dryRun,
				checkMerged:    checkMerged,
				checkUpstream:  checkUpstream,
				force:          force,
				upstreamBranch: upstreamBranch,
			})
		},
	}

	cmd.Flags().StringVarP(&worktreeDir, "worktree", "w", "/tmp/gh-resolve-conflicts-worktrees", "Directory containing worktrees")
	cmd.Flags().BoolVarP(&dryRun, "dry-run", "n", false, "Show what would be removed without removing")
	cmd.Flags().BoolVarP(&checkMerged, "check-merged", "m", false, "Only remove worktrees for merged/closed PRs")
	cmd.Flags().BoolVarP(&checkUpstream, "check-upstream", "u", false, "Check if commits are merged upstream")
	cmd.Flags().StringVarP(&upstreamBranch, "upstream-branch", "b", "main", "Upstream branch to check against")
	cmd.Flags().BoolVarP(&force, "force", "f", false, "Force remove even with uncommitted changes")

	return cmd
}

type cleanupOpts struct {
	worktreeDir    string
	dryRun         bool
	checkMerged    bool
	checkUpstream  bool
	force          bool
	upstreamBranch string
}

type mergeStatus struct {
	prMerged        bool
	upstreamMerged  bool
	prChecked       bool
	upstreamChecked bool
}

type worktreeInfo struct {
	path   string
	branch string
	prNum  string
	repo   string
}

func runCleanup(out *output.Writer, opts cleanupOpts) error {
	if _, err := os.Stat(opts.worktreeDir); os.IsNotExist(err) {
		out.Warning("Directory does not exist: %s", opts.worktreeDir)
		return nil
	}

	out.Info("Scanning for git worktrees in: %s", opts.worktreeDir)
	fmt.Println()

	// Find all directories that might contain git repos
	entries, err := os.ReadDir(opts.worktreeDir)
	if err != nil {
		return fmt.Errorf("failed to read directory: %w", err)
	}

	totalRemoved := 0
	totalKept := 0

	for _, entry := range entries {
		if !entry.IsDir() {
			continue
		}

		repoName := entry.Name()
		repoPath := filepath.Join(opts.worktreeDir, repoName)

		// Look for main directory (the base repo)
		mainPath := filepath.Join(repoPath, "main")
		if _, err := os.Stat(filepath.Join(mainPath, ".git")); os.IsNotExist(err) {
			continue
		}

		out.Info("📁 Repository: %s", repoName)

		// Get list of worktrees
		worktrees, err := getWorktrees(mainPath)
		if err != nil {
			out.Error("Failed to list worktrees: %v", err)
			continue
		}

		if len(worktrees) == 0 {
			out.Info("  No worktrees found")
			fmt.Println()
			continue
		}

		// Process each worktree
		for _, wt := range worktrees {
			wt.repo = repoName
			removed, kept, err := processWorktree(out, mainPath, wt, opts)
			if err != nil {
				out.Error("  Failed to process worktree %s: %v", wt.path, err)
				continue
			}
			totalRemoved += removed
			totalKept += kept
		}

		fmt.Println()
	}

	// Summary
	if opts.dryRun {
		out.Success("Dry run complete. Would remove %d worktrees, keeping %d", totalRemoved, totalKept)
	} else {
		out.Success("Cleanup complete! Removed %d worktrees, kept %d", totalRemoved, totalKept)
	}

	return nil
}

func getWorktrees(mainPath string) ([]worktreeInfo, error) {
	cmd := exec.Command("git", "-C", mainPath, "worktree", "list", "--porcelain")
	output, err := cmd.Output()
	if err != nil {
		return nil, err
	}

	var worktrees []worktreeInfo
	var current worktreeInfo
	prPattern := regexp.MustCompile(`^pr-(\d+)$`)

	for _, line := range strings.Split(string(output), "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			if current.path != "" && !strings.HasSuffix(current.path, "/main") {
				// Extract PR number from branch name if it matches pr-XXX
				if matches := prPattern.FindStringSubmatch(current.branch); len(matches) > 1 {
					current.prNum = matches[1]
				}
				worktrees = append(worktrees, current)
			}
			current = worktreeInfo{}
			continue
		}

		if strings.HasPrefix(line, "worktree ") {
			current.path = strings.TrimPrefix(line, "worktree ")
		} else if strings.HasPrefix(line, "branch ") {
			branch := strings.TrimPrefix(line, "branch ")
			// Remove refs/heads/ prefix
			current.branch = strings.TrimPrefix(branch, "refs/heads/")
		}
	}

	// Don't forget the last one
	if current.path != "" && !strings.HasSuffix(current.path, "/main") {
		if matches := prPattern.FindStringSubmatch(current.branch); len(matches) > 1 {
			current.prNum = matches[1]
		}
		worktrees = append(worktrees, current)
	}

	return worktrees, nil
}

func processWorktree(out *output.Writer, mainPath string, wt worktreeInfo, opts cleanupOpts) (removed int, kept int, err error) {
	name := filepath.Base(wt.path)

	// Check if worktree still exists
	if _, err := os.Stat(wt.path); os.IsNotExist(err) {
		out.Warning("  ⚠️  %s [%s] - path does not exist, pruning", name, wt.branch)
		if !opts.dryRun {
			cmd := exec.Command("git", "-C", mainPath, "worktree", "prune")
			if err := cmd.Run(); err != nil {
				return 0, 0, fmt.Errorf("failed to prune: %w", err)
			}
		}
		return 1, 0, nil
	}

	// Check for uncommitted changes
	hasChanges, err := hasUncommittedChanges(wt.path)
	if err != nil {
		return 0, 0, err
	}

	if hasChanges && !opts.force {
		out.Warning("  ⚠️  %s [%s] - has uncommitted changes (keeping)", name, wt.branch)
		return 0, 1, nil
	}

	// Check merge status
	status := mergeStatus{}

	// Check if PR is merged/closed (if requested)
	if opts.checkMerged && wt.prNum != "" {
		merged, err := isPRMergedOrClosed(wt.repo, wt.prNum)
		if err != nil {
			out.Warning("  ⚠️  %s [%s] - failed to check PR status: %v (keeping)", name, wt.branch, err)
			return 0, 1, nil
		}
		status.prMerged = merged
		status.prChecked = true
	}

	// Check if commits are merged upstream (if requested)
	if opts.checkUpstream {
		merged, err := isBranchMergedUpstream(mainPath, wt.branch, opts.upstreamBranch)
		if err != nil {
			// Don't fail, just note we couldn't check
			status.upstreamChecked = false
		} else {
			status.upstreamMerged = merged
			status.upstreamChecked = true
		}
	}

	// Decide whether to keep or remove based on status
	shouldRemove, reason := shouldRemoveWorktree(status, opts, wt)

	if !shouldRemove {
		printKeepStatus(out, name, wt.branch, wt.prNum, status, reason)
		return 0, 1, nil
	}

	// Remove the worktree
	printRemoveStatus(out, name, wt.branch, wt.prNum, status, opts.dryRun)

	if !opts.dryRun {
		cmd := exec.Command("git", "-C", mainPath, "worktree", "remove", wt.path)
		if err := cmd.Run(); err != nil {
			// Try with --force
			out.Warning("     Retrying with --force...")
			cmd = exec.Command("git", "-C", mainPath, "worktree", "remove", "--force", wt.path)
			if err := cmd.Run(); err != nil {
				return 0, 0, fmt.Errorf("failed to remove: %w", err)
			}
		}
	}

	return 1, 0, nil
}

func shouldRemoveWorktree(status mergeStatus, opts cleanupOpts, wt worktreeInfo) (bool, string) {
	// If we're checking merge status, only remove if something is merged
	if opts.checkMerged || opts.checkUpstream {
		if status.prChecked && status.prMerged {
			return true, "PR merged"
		}
		if status.upstreamChecked && status.upstreamMerged {
			return true, "commits merged upstream"
		}
		// Neither merged, keep it
		if status.prChecked && !status.prMerged {
			return false, "PR still open"
		}
		if status.upstreamChecked && !status.upstreamMerged {
			return false, "not merged upstream"
		}
		return false, "merge status unknown"
	}

	// If not checking merge status, remove all clean worktrees
	return true, "clean"
}

func printKeepStatus(out *output.Writer, name, branch, prNum string, status mergeStatus, reason string) {
	prLabel := ""
	if prNum != "" {
		prLabel = fmt.Sprintf(" PR #%s", prNum)
	}

	statusInfo := ""
	if status.upstreamChecked {
		if status.upstreamMerged {
			statusInfo = " [merged upstream]"
		} else {
			statusInfo = " [not merged upstream]"
		}
	}
	if status.prChecked {
		if status.prMerged {
			statusInfo += " [PR merged]"
		} else {
			statusInfo += " [PR open]"
		}
	}

	out.Info("  ℹ️  %s [%s]%s%s - %s (keeping)", name, branch, prLabel, statusInfo, reason)
}

func printRemoveStatus(out *output.Writer, name, branch, prNum string, status mergeStatus, dryRun bool) {
	prLabel := ""
	if prNum != "" {
		prLabel = fmt.Sprintf(" PR #%s", prNum)
	}

	action := "removing"
	if dryRun {
		action = "would remove"
	}

	// Use different emojis/colors based on merge status
	if status.upstreamMerged && status.upstreamChecked {
		// Merged upstream - use checkmark (green by default via Success)
		fmt.Printf("  ✅ %s [%s]%s - merged upstream (%s)\n", name, branch, prLabel, action)
	} else if status.prMerged && status.prChecked {
		// PR merged but maybe not upstream yet - use green circle
		fmt.Printf("  ✅ %s [%s]%s - PR merged (%s)\n", name, branch, prLabel, action)
	} else {
		// Clean but not verified as merged - use trash can (standard removal)
		fmt.Printf("  🗑️  %s [%s]%s - clean (%s)\n", name, branch, prLabel, action)
	}
}

func hasUncommittedChanges(path string) (bool, error) {
	cmd := exec.Command("git", "-C", path, "status", "--porcelain")
	output, err := cmd.Output()
	if err != nil {
		return false, err
	}
	return len(strings.TrimSpace(string(output))) > 0, nil
}

func isPRMergedOrClosed(repo, prNum string) (bool, error) {
	// Use gh to check PR state
	cmd := exec.Command("gh", "pr", "view", prNum, "--repo", repo, "--json", "state", "--jq", ".state")
	output, err := cmd.Output()
	if err != nil {
		return false, err
	}

	state := strings.TrimSpace(string(output))
	return state == "MERGED" || state == "CLOSED", nil
}

func isBranchMergedUpstream(repoPath, branch, upstreamBranch string) (bool, error) {
	// First, check if the branch exists
	checkCmd := exec.Command("git", "-C", repoPath, "rev-parse", "--verify", branch)
	if err := checkCmd.Run(); err != nil {
		// Branch doesn't exist, consider it merged/deleted
		return true, nil
	}

	// Check if there are commits in the branch that are not in upstream
	// git cherry returns commits that exist in branch but not in upstream
	// Empty output means all commits are merged
	cmd := exec.Command("git", "-C", repoPath, "cherry", upstreamBranch, branch)
	output, err := cmd.Output()
	if err != nil {
		// If cherry fails, try a different approach: check if branch is ancestor of upstream
		mergeBaseCmd := exec.Command("git", "-C", repoPath, "merge-base", "--is-ancestor", branch, upstreamBranch)
		if mergeBaseErr := mergeBaseCmd.Run(); mergeBaseErr == nil {
			// Branch is ancestor of upstream, so it's merged
			return true, nil
		}
		return false, err
	}

	// If output is empty, all commits from branch are in upstream
	result := strings.TrimSpace(string(output))
	return result == "", nil
}
