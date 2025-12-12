package main

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"strings"

	"github.com/spf13/cobra"
	"github.com/vdemeester/home/tools/gh-pr/internal/conflicts"
	"github.com/vdemeester/home/tools/gh-pr/internal/output"
)

func resolveConflictsCmd(out *output.Writer) *cobra.Command {
	var (
		worktreeDir string
		noWorktree  bool
		noPush      bool
		org         string
		author      string
	)

	cmd := &cobra.Command{
		Use:   "resolve-conflicts [REPOSITORY[#PR_NUMBER]]",
		Short: "Resolve merge conflicts in pull requests",
		Long: `List pull requests with merge conflicts and resolve them interactively.

This command helps you resolve merge conflicts in pull requests by:
  - Fetching the PR branch
  - Creating a worktree or using existing repo
  - Performing rebase against the base branch
  - Launching conflict resolution tools (emacs ediff or git mergetool)
  - Force-pushing resolved changes (optional)

Examples:
  gh-pr resolve-conflicts                 # Search for your conflicting PRs
  gh-pr resolve-conflicts owner/repo#123  # Resolve specific PR
  gh-pr resolve-conflicts -o tektoncd     # Filter by organization
  gh-pr resolve-conflicts -n              # Use existing repo, no worktree
  gh-pr resolve-conflicts -N              # Don't auto-push after resolution`,
		RunE: func(cmd *cobra.Command, args []string) error {
			var repo, prNumber string

			if len(args) > 0 {
				arg := args[0]
				if strings.Contains(arg, "#") {
					parts := strings.SplitN(arg, "#", 2)
					repo = parts[0]
					prNumber = parts[1]
				} else {
					repo = arg
				}
			}

			return runResolveConflicts(out, resolveConflictsOpts{
				worktreeDir: worktreeDir,
				useWorktree: !noWorktree,
				autoPush:    !noPush,
				org:         org,
				author:      author,
				repo:        repo,
				prNumber:    prNumber,
			})
		},
	}

	cmd.Flags().StringVarP(&worktreeDir, "worktree", "w", "/tmp/gh-resolve-conflicts-worktrees", "Create worktrees in DIR")
	cmd.Flags().BoolVarP(&noWorktree, "no-worktree", "n", false, "Use existing repo instead of worktrees")
	cmd.Flags().BoolVarP(&noPush, "no-push", "N", false, "Do NOT auto-push after resolution")
	cmd.Flags().StringVarP(&org, "org", "o", "", "Filter PRs by organization")
	cmd.Flags().StringVarP(&author, "author", "a", "@me", "Filter PRs by author")

	return cmd
}

type resolveConflictsOpts struct {
	worktreeDir string
	useWorktree bool
	autoPush    bool
	org         string
	author      string
	repo        string
	prNumber    string
}

func runResolveConflicts(out *output.Writer, opts resolveConflictsOpts) error {
	resolver := conflicts.NewResolver(out, opts.worktreeDir, opts.useWorktree, opts.autoPush)

	// If specific PR is provided, resolve it directly
	if opts.prNumber != "" {
		if opts.repo == "" {
			return fmt.Errorf("repository must be specified when using #PR_NUMBER")
		}

		pr, err := resolver.FindConflictingPR(opts.repo, opts.prNumber)
		if err != nil {
			return err
		}

		out.Success("PR #%d: %s", pr.Number, pr.Title)
		out.Println("")

		return resolver.ResolvePR(opts.repo, pr)
	}

	// Interactive mode: search for conflicting PRs
	prs, err := resolver.FindConflictingPRs(opts.org, opts.author)
	if err != nil {
		return err
	}

	if len(prs) == 0 {
		out.Success("No pull requests with merge conflicts found!")
		return nil
	}

	out.Success("Found %d pull request(s) with merge conflicts", len(prs))

	// Check if fzf is available
	if _, err := exec.LookPath("fzf"); err != nil {
		return fmt.Errorf("fzf is required but not found in PATH: %w", err)
	}

	// Build fzf input with formatted PR information
	var fzfInput strings.Builder
	prMap := make(map[int]*conflicts.PRInfo) // Map PR number to info for later lookup

	for _, pr := range prs {
		prMap[pr.Number] = &pr
		// Format: "#123  Title here (@author) [repo]"
		repo := pr.HeadRepository.NameWithOwner
		fzfInput.WriteString(fmt.Sprintf("#%-6d %s (@%s) [%s]\n",
			pr.Number, pr.Title, pr.Author.Login, repo))
	}

	// Build preview command for fzf
	// We need to extract the repo from the selection and fetch PR details
	previewCmd := `echo {} | awk '{print $1, $(NF)}' | sed 's/\[//;s/\]//' | \
		xargs -I{} sh -c 'PR=$(echo {} | cut -d" " -f1); REPO=$(echo {} | cut -d" " -f2); \
		gh pr view $PR -R $REPO --json number,title,author,headRefName,baseRefName,mergeable,statusCheckRollup 2>/dev/null | \
		jq -r "\"# PR \" + (.number | tostring) + \": \" + .title,
		       \"\",
		       \"Author: @\" + .author.login,
		       \"\",
		       \"## Branches:\",
		       \"\",
		       \"  \" + .headRefName + \" → \" + .baseRefName,
		       \"\",
		       \"## Merge Status: \" + (.mergeable // \"unknown\"),
		       \"\",
		       \"## Status Checks:\",
		       \"\",
		       (if (.statusCheckRollup // [] | length) == 0 then \"  (No status checks)\"
		        else (.statusCheckRollup | map(
		         \"  \" + (
		           if .conclusion == \"SUCCESS\" then \"✓\"
		           elif .conclusion == \"FAILURE\" then \"✗\"
		           elif .conclusion == \"PENDING\" then \"●\"
		           elif .conclusion == \"SKIPPED\" then \"○\"
		           else \"?\"
		           end
		         ) + \" \" + .name + \" (\" + (.conclusion // \"unknown\") + \")\"
		       ) | join(\"\\n\"))
		        end)"'`

	// Use fzf for multi-select with preview
	out.Info("Select pull requests to resolve (use Tab to select multiple, Enter to confirm)...")
	fzfCmd := exec.Command("fzf",
		"--multi",
		"--ansi",
		"--header", "Select PRs to resolve conflicts (Tab: select, Enter: confirm)",
		"--preview", previewCmd,
		"--preview-window", "right:60%:wrap",
	)
	fzfCmd.Stdin = strings.NewReader(fzfInput.String())
	fzfCmd.Stderr = os.Stderr

	selectedOutput, err := fzfCmd.Output()
	if err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok && exitErr.ExitCode() == 130 {
			// User cancelled with Ctrl+C
			out.Info("Selection cancelled.")
			return nil
		}
		return fmt.Errorf("fzf selection failed: %w", err)
	}

	selectedPRs := strings.TrimSpace(string(selectedOutput))
	if selectedPRs == "" {
		out.Info("No pull requests selected.")
		return nil
	}

	// Extract PR numbers from selected lines
	selectedPRNumbers := []int{}
	scanner := bufio.NewScanner(strings.NewReader(selectedPRs))
	for scanner.Scan() {
		line := scanner.Text()
		// Extract PR number from format "#123  Title..."
		if strings.HasPrefix(line, "#") {
			var prNum int
			if _, err := fmt.Sscanf(line, "#%d", &prNum); err == nil {
				selectedPRNumbers = append(selectedPRNumbers, prNum)
			}
		}
	}

	if len(selectedPRNumbers) == 0 {
		out.Warning("No valid PR numbers found in selection.")
		return nil
	}

	out.Success("Selected %d pull request(s)", len(selectedPRNumbers))
	out.Println("")

	// Resolve selected PRs
	for _, prNum := range selectedPRNumbers {
		pr, ok := prMap[prNum]
		if !ok {
			out.Warning("PR #%d not found in the list, skipping...", prNum)
			continue
		}

		// Determine repository from PR
		repo := pr.HeadRepository.NameWithOwner
		if pr.IsCrossRepository {
			out.Warning("Skipping cross-repository PR #%d (requires manual handling)", prNum)
			continue
		}

		if err := resolver.ResolvePR(repo, pr); err != nil {
			out.Error("Failed to resolve PR #%d: %v", prNum, err)
			out.Println("")
			continue
		}

		out.Success("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
		out.Println("")
	}

	out.Success("Done!")
	return nil
}
