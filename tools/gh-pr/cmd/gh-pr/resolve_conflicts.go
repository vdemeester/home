package main

import (
	"fmt"
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

	out.Warning("Found %d pull request(s) with merge conflicts:", len(prs))
	out.Println("")

	// Display PRs
	for i, pr := range prs {
		out.Println("%d. PR #%d: %s (@%s)", i+1, pr.Number, pr.Title, pr.Author.Login)
	}

	out.Println("")
	out.Info("Processing conflicting pull requests...")
	out.Println("")

	// Resolve each PR
	for _, pr := range prs {
		// Determine repository from PR
		repo := fmt.Sprintf("%s/%s", pr.HeadRepositoryOwner.Login, pr.HeadRepository.Name)
		if pr.IsCrossRepository {
			// For cross-repo PRs, we need the base repo
			// This is a limitation - we'd need to track base repo in search results
			out.Warning("Skipping cross-repository PR #%d (requires base repo info)", pr.Number)
			continue
		}

		if err := resolver.ResolvePR(repo, &pr); err != nil {
			out.Error("Failed to resolve PR #%d: %v", pr.Number, err)
			out.Println("")
			continue
		}

		out.Success("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
		out.Println("")
	}

	out.Success("Done!")
	return nil
}
