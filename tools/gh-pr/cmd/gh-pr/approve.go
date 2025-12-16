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

func approveCmd(out *output.Writer) *cobra.Command {
	var (
		prow          bool
		merge         bool
		force         bool
		comment       string
		editorComment bool
		interactive   bool
	)

	cmd := &cobra.Command{
		Use:   "approve [projects...]",
		Short: "Approve pull requests",
		Long: `Select and approve pull requests using fzf.

This command lists pull requests from specified GitHub repositories and allows
you to select and approve them interactively. You can optionally merge approved
PRs and add custom comments.

If no projects are specified, uses default Tekton projects:
- tektoncd/pipeline
- tektoncd/plumbing
- tektoncd/cli
- tektoncd/mcp-server

Examples:
  # Approve PRs from default projects
  gh-pr approve

  # Approve PRs from specific projects
  gh-pr approve tektoncd/pipeline tektoncd/cli

  # Approve with Prow comment
  gh-pr approve -p tektoncd/pipeline

  # Approve and merge
  gh-pr approve -m tektoncd/pipeline

  # Approve with custom comment
  gh-pr approve -c "LGTM! Great work" tektoncd/pipeline

  # Approve with editor for multi-line comment
  gh-pr approve -C tektoncd/pipeline

  # Force merge (requires admin)
  gh-pr approve -m -f tektoncd/pipeline`,
		RunE: func(cmd *cobra.Command, args []string) error {
			projects := args
			if len(projects) == 0 {
				// Use default Tekton projects
				projects = []string{
					"tektoncd/pipeline",
					"tektoncd/plumbing",
					"tektoncd/cli",
					"tektoncd/mcp-server",
				}
			}

			return runApprove(out, approveOpts{
				projects:      projects,
				prow:          prow,
				merge:         merge,
				force:         force,
				comment:       comment,
				editorComment: editorComment,
				interactive:   interactive,
			})
		},
	}

	cmd.Flags().BoolVarP(&prow, "prow", "p", false, "Add Prow /lgtm comment")
	cmd.Flags().BoolVarP(&merge, "merge", "m", false, "Merge PR after approval")
	cmd.Flags().BoolVarP(&force, "force", "f", false, "Force merge with --admin (requires admin rights)")
	cmd.Flags().StringVarP(&comment, "comment", "c", "", "Custom approval comment")
	cmd.Flags().BoolVarP(&editorComment, "editor", "C", false, "Open editor for multi-line comment")
	cmd.Flags().BoolVarP(&interactive, "interactive", "i", false, "Prompt for comment interactively")

	return cmd
}

type approveOpts struct {
	projects      []string
	prow          bool
	merge         bool
	force         bool
	comment       string
	editorComment bool
	interactive   bool
}

func runApprove(out *output.Writer, opts approveOpts) error {
	// Check if fzf is available
	if _, err := exec.LookPath("fzf"); err != nil {
		return fmt.Errorf("fzf is required but not found in PATH: %w", err)
	}

	// Handle comment options
	commentBody := opts.comment
	if opts.editorComment {
		body, err := getEditorComment()
		if err != nil {
			return fmt.Errorf("failed to get editor comment: %w", err)
		}
		commentBody = body
	} else if commentBody == "" && opts.interactive {
		out.Info("Enter approval comment (press Enter to skip):")
		reader := bufio.NewReader(os.Stdin)
		line, err := reader.ReadString('\n')
		if err != nil {
			return fmt.Errorf("failed to read comment: %w", err)
		}
		commentBody = strings.TrimSpace(line)
	}

	// Process each project
	for _, project := range opts.projects {
		out.Info("Processing project: %s", project)

		if err := processProject(out, project, commentBody, opts); err != nil {
			out.Error("Failed to process project %s: %v", project, err)
			continue
		}
	}

	return nil
}

func processProject(out *output.Writer, project, commentBody string, opts approveOpts) error {
	// Build gh pr list command
	ghArgs := []string{"pr", "list", "--repo", project, "--json", "number,title,author"}

	ghCmd := exec.Command("gh", ghArgs...)
	ghOutput, err := ghCmd.Output()
	if err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return fmt.Errorf("gh pr list failed: %s", exitErr.Stderr)
		}
		return fmt.Errorf("gh pr list failed: %w", err)
	}

	prListJSON := strings.TrimSpace(string(ghOutput))
	if prListJSON == "" || prListJSON == "[]" {
		out.Warning("No pull requests found in %s", project)
		return nil
	}

	// Format PR list for fzf using jq
	jqCmd := exec.Command("jq", "-r",
		`.[] | (.number | tostring) + "\t" + .author.login + "\t" + .title`)
	jqCmd.Stdin = strings.NewReader(prListJSON)
	jqOutput, err := jqCmd.Output()
	if err != nil {
		return fmt.Errorf("failed to format PR list: %w", err)
	}

	formattedList := strings.TrimSpace(string(jqOutput))
	if formattedList == "" {
		out.Warning("No pull requests to display in %s", project)
		return nil
	}

	// Use column to align the output
	columnCmd := exec.Command("column", "-t", "-s", "\t")
	columnCmd.Stdin = strings.NewReader(formattedList)
	columnOutput, err := columnCmd.Output()
	if err != nil {
		// If column fails, use the original formatted list
		columnOutput = []byte(formattedList)
	}

	// Build preview command for fzf
	previewCmd := fmt.Sprintf(`gh pr checks --repo=%s {1} --json 'name,state' 2>/dev/null | \
		jq -r 'map(.state + ": " + .name) | .[]' || echo "No checks found"`, project)

	// Use fzf for multi-select with preview
	out.Info("Select PRs to approve from %s (Tab: multi-select, Enter: confirm)...", project)
	fzfCmd := exec.Command("fzf",
		"--multi",
		"--ansi",
		"--header", fmt.Sprintf("Select PRs from %s (Tab: select, Enter: confirm)", project),
		"--preview", previewCmd,
		"--preview-window", "right:50%:wrap",
	)
	fzfCmd.Stdin = bytes.NewReader(columnOutput)
	fzfCmd.Stderr = os.Stderr

	selectedOutput, err := fzfCmd.Output()
	if err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok && exitErr.ExitCode() == 130 {
			// User cancelled with Ctrl+C
			out.Info("Selection cancelled for %s", project)
			return nil
		}
		return fmt.Errorf("fzf selection failed: %w", err)
	}

	selectedPRs := strings.TrimSpace(string(selectedOutput))
	if selectedPRs == "" {
		out.Info("No pull requests selected from %s", project)
		return nil
	}

	// Extract PR numbers from selected lines
	prNumbers := []string{}
	scanner := bufio.NewScanner(strings.NewReader(selectedPRs))
	for scanner.Scan() {
		line := scanner.Text()
		// Extract PR number from the first field
		fields := strings.Fields(line)
		if len(fields) > 0 {
			prNumbers = append(prNumbers, fields[0])
		}
	}

	if len(prNumbers) == 0 {
		out.Warning("No valid PR numbers found in selection")
		return nil
	}

	out.Success("Selected %d pull request(s): %s", len(prNumbers), strings.Join(prNumbers, ", "))

	// Approve each PR
	failed := []string{}
	for _, prNum := range prNumbers {
		if err := approvePR(out, project, prNum, commentBody, opts); err != nil {
			out.Error("Failed to approve PR #%s: %v", prNum, err)
			failed = append(failed, prNum)
			continue
		}

		// Merge if requested
		if opts.merge {
			if err := mergePR(out, project, prNum, commentBody, opts.force); err != nil {
				out.Error("Failed to merge PR #%s: %v", prNum, err)
				failed = append(failed, prNum)
			}
		}
	}

	// Summary
	successCount := len(prNumbers) - len(failed)
	if successCount > 0 {
		out.Success("\nSuccessfully processed %d pull request(s) from %s", successCount, project)
	}
	if len(failed) > 0 {
		out.Error("Failed to process %d pull request(s) from %s: %s",
			len(failed), project, strings.Join(failed, ", "))
	}

	return nil
}

func approvePR(out *output.Writer, repo, prNum, commentBody string, opts approveOpts) error {
	// Build review body
	reviewBody := commentBody
	if opts.prow {
		if reviewBody != "" {
			reviewBody = "/lgtm\n\n" + reviewBody
		} else {
			reviewBody = "/lgtm"
		}
	}

	// Build gh pr review command
	reviewArgs := []string{"pr", "review", "--repo", repo, prNum, "--approve"}
	if reviewBody != "" {
		reviewArgs = append(reviewArgs, "--body", reviewBody)
	}

	reviewCmd := exec.Command("gh", reviewArgs...)
	if output, err := reviewCmd.CombinedOutput(); err != nil {
		return fmt.Errorf("gh pr review failed: %s", output)
	}

	out.Success("✓ Approved PR #%s", prNum)
	return nil
}

func mergePR(out *output.Writer, repo, prNum, commentBody string, force bool) error {
	// Build merge args
	mergeArgs := []string{"pr", "merge", "--repo", repo}

	if force {
		mergeArgs = append(mergeArgs, "--admin")
		out.Info("Merging PR #%s with admin privileges...", prNum)
	} else {
		mergeArgs = append(mergeArgs, "--auto")
	}

	mergeArgs = append(mergeArgs, "--rebase", "--delete-branch")

	if commentBody != "" {
		mergeArgs = append(mergeArgs, "--body", commentBody)
	}

	mergeArgs = append(mergeArgs, prNum)

	// Execute merge
	mergeCmd := exec.Command("gh", mergeArgs...)
	if output, err := mergeCmd.CombinedOutput(); err != nil {
		return fmt.Errorf("gh pr merge failed: %s", output)
	}

	out.Success("✓ Merged PR #%s", prNum)
	return nil
}

func getEditorComment() (string, error) {
	// Create temporary file
	tmpfile, err := os.CreateTemp("", "gh-pr-comment-*.md")
	if err != nil {
		return "", fmt.Errorf("failed to create temp file: %w", err)
	}
	defer os.Remove(tmpfile.Name())

	// Write template
	template := `# Enter your approval comment below
# Lines starting with # will be ignored
# Save and close the editor to continue

`
	if _, err := tmpfile.WriteString(template); err != nil {
		return "", fmt.Errorf("failed to write template: %w", err)
	}
	tmpfile.Close()

	// Get editor
	editor := os.Getenv("EDITOR")
	if editor == "" {
		editor = "vim"
	}

	// Open editor
	editorCmd := exec.Command(editor, tmpfile.Name())
	editorCmd.Stdin = os.Stdin
	editorCmd.Stdout = os.Stdout
	editorCmd.Stderr = os.Stderr

	if err := editorCmd.Run(); err != nil {
		return "", fmt.Errorf("editor failed: %w", err)
	}

	// Read back the file
	content, err := os.ReadFile(tmpfile.Name())
	if err != nil {
		return "", fmt.Errorf("failed to read comment file: %w", err)
	}

	// Filter out comment lines and empty lines
	var lines []string
	scanner := bufio.NewScanner(bytes.NewReader(content))
	for scanner.Scan() {
		line := scanner.Text()
		if !strings.HasPrefix(line, "#") && strings.TrimSpace(line) != "" {
			lines = append(lines, line)
		}
	}

	return strings.Join(lines, "\n"), nil
}
