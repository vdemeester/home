package main

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"strings"

	"github.com/spf13/cobra"
	"github.com/vdemeester/home/tools/gh-pr/internal/output"
)

func commentCmd(out *output.Writer) *cobra.Command {
	var (
		body   string
		repo   string
		labels []string
		author string
		state  string
	)

	cmd := &cobra.Command{
		Use:   "comment",
		Short: "Comment on multiple pull requests",
		Long: `Select and comment on multiple pull requests using fzf.

This command lists pull requests and allows you to select multiple PRs
using fzf's multi-select feature. You can then add the same comment to
all selected PRs.

Examples:
  gh-pr comment                           # Select PRs interactively
  gh-pr comment --body "LGTM"            # Pre-specify the comment
  gh-pr comment --label bug              # Filter by label
  gh-pr comment --repo owner/repo        # Work with a specific repo
  gh-pr comment --state all              # Include closed PRs`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return runComment(out, commentOpts{
				body:   body,
				repo:   repo,
				labels: labels,
				author: author,
				state:  state,
			})
		},
	}

	cmd.Flags().StringVarP(&body, "body", "b", "", "Comment body (will prompt if not provided)")
	cmd.Flags().StringVarP(&repo, "repo", "R", "", "Repository (owner/repo format)")
	cmd.Flags().StringSliceVarP(&labels, "label", "l", nil, "Filter PRs by label (comma-separated)")
	cmd.Flags().StringVarP(&author, "author", "a", "", "Filter PRs by author")
	cmd.Flags().StringVarP(&state, "state", "s", "open", "Filter by state: open, closed, merged, all")

	return cmd
}

type commentOpts struct {
	body   string
	repo   string
	labels []string
	author string
	state  string
}

func runComment(out *output.Writer, opts commentOpts) error {
	// Check if fzf is available
	if _, err := exec.LookPath("fzf"); err != nil {
		return fmt.Errorf("fzf is required but not found in PATH: %w", err)
	}

	// Build gh pr list command
	ghArgs := []string{"pr", "list"}

	if opts.repo != "" {
		ghArgs = append(ghArgs, "--repo", opts.repo)
	}

	for _, label := range opts.labels {
		ghArgs = append(ghArgs, "--label", label)
	}

	if opts.author != "" {
		ghArgs = append(ghArgs, "--author", opts.author)
	}

	if opts.state != "" {
		ghArgs = append(ghArgs, "--state", opts.state)
	}

	// Get list of PRs
	out.Info("Fetching pull requests...")
	ghCmd := exec.Command("gh", ghArgs...)
	ghOutput, err := ghCmd.Output()
	if err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return fmt.Errorf("gh pr list failed: %s", exitErr.Stderr)
		}
		return fmt.Errorf("gh pr list failed: %w", err)
	}

	prList := strings.TrimSpace(string(ghOutput))
	if prList == "" {
		out.Warning("No pull requests found matching the criteria.")
		return nil
	}

	// Build preview command for fzf
	repoFlag := ""
	if opts.repo != "" {
		repoFlag = fmt.Sprintf("-R %s", opts.repo)
	}

	previewCmd := fmt.Sprintf(`gh pr view {1} %s --json number,title,author,statusCheckRollup | \
		jq -r '"# PR " + (.number | tostring) + ": " + .title,
		       "",
		       "Author: @" + .author.login,
		       "",
		       "## Status Checks:",
		       "",
		       (if (.statusCheckRollup // [] | length) == 0 then "  (No status checks)"
		        else (.statusCheckRollup | map(
		         "  " + (
		           if .conclusion == "SUCCESS" then "✓"
		           elif .conclusion == "FAILURE" then "✗"
		           elif .conclusion == "PENDING" then "●"
		           elif .conclusion == "SKIPPED" then "○"
		           else "?"
		           end
		         ) + " " + .name + " (" + (.conclusion // "unknown") + ")"
		       ) | join("\n"))
		        end)'`, repoFlag)

	// Use fzf for multi-select with preview
	out.Info("Select pull requests (use Tab to select multiple, Enter to confirm)...")
	fzfCmd := exec.Command("fzf",
		"--multi",
		"--ansi",
		"--header", "Select PRs (Tab: select, Enter: confirm)",
		"--preview", previewCmd,
		"--preview-window", "right:60%:wrap",
	)
	fzfCmd.Stdin = strings.NewReader(prList)
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
	prNumbers := []string{}
	scanner := bufio.NewScanner(strings.NewReader(selectedPRs))
	for scanner.Scan() {
		line := scanner.Text()
		// Extract PR number from the first field (format: "123  title...")
		fields := strings.Fields(line)
		if len(fields) > 0 {
			prNumbers = append(prNumbers, fields[0])
		}
	}

	if len(prNumbers) == 0 {
		out.Warning("No valid PR numbers found in selection.")
		return nil
	}

	out.Success("Selected %d pull request(s): %s", len(prNumbers), strings.Join(prNumbers, ", "))

	// Get comment body if not provided
	commentBody := opts.body
	if commentBody == "" {
		out.Info("Enter your comment (Ctrl+D when done):")
		var buf bytes.Buffer
		scanner := bufio.NewScanner(os.Stdin)
		for scanner.Scan() {
			buf.WriteString(scanner.Text())
			buf.WriteString("\n")
		}
		if err := scanner.Err(); err != nil {
			return fmt.Errorf("failed to read comment: %w", err)
		}
		commentBody = strings.TrimSpace(buf.String())
	}

	if commentBody == "" {
		out.Warning("Empty comment body. No comments will be posted.")
		return nil
	}

	// Comment on each PR
	out.Info("Posting comment to %d pull request(s)...", len(prNumbers))
	failed := []string{}

	for _, prNum := range prNumbers {
		commentArgs := []string{"pr", "comment", prNum, "--body", commentBody}
		if opts.repo != "" {
			commentArgs = append(commentArgs, "--repo", opts.repo)
		}

		commentCmd := exec.Command("gh", commentArgs...)
		if err := commentCmd.Run(); err != nil {
			out.Error("Failed to comment on PR #%s: %v", prNum, err)
			failed = append(failed, prNum)
		} else {
			out.Success("Commented on PR #%s", prNum)
		}
	}

	// Summary
	successCount := len(prNumbers) - len(failed)
	if successCount > 0 {
		out.Success("\nSuccessfully commented on %d pull request(s)", successCount)
	}
	if len(failed) > 0 {
		out.Error("Failed to comment on %d pull request(s): %s", len(failed), strings.Join(failed, ", "))
		return fmt.Errorf("some comments failed")
	}

	return nil
}
