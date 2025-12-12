package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"

	"github.com/spf13/cobra"
	"github.com/vdemeester/home/tools/gh-pr/internal/output"
)

func restartFailedCmd(out *output.Writer) *cobra.Command {
	var (
		ignorePatterns []string
		labels         []string
		repo           string
		prNumber       string
	)

	cmd := &cobra.Command{
		Use:   "restart-failed [REPOSITORY[#PR_NUMBER]]",
		Short: "Restart failed workflow runs on pull requests",
		Long: `List pull requests with failed checks and restart selected workflows.

By default, "Label Checker" workflows are ignored. Use --ignore to add more patterns.

Examples:
  gh-pr restart-failed                     # Interactive mode
  gh-pr restart-failed owner/repo#123      # Restart specific PR
  gh-pr restart-failed --ignore build      # Ignore "build" workflows
  gh-pr restart-failed --label bug         # Filter by label`,
		RunE: func(cmd *cobra.Command, args []string) error {
			// Parse repository argument
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

			return runRestartFailed(out, restartFailedOpts{
				ignorePatterns: append([]string{"Label Checker"}, ignorePatterns...),
				labels:         labels,
				repo:           repo,
				prNumber:       prNumber,
			})
		},
	}

	cmd.Flags().StringSliceVarP(&ignorePatterns, "ignore", "i", nil, "Ignore workflows matching pattern")
	cmd.Flags().StringSliceVarP(&labels, "label", "l", nil, "Filter PRs by label")

	return cmd
}

type restartFailedOpts struct {
	ignorePatterns []string
	labels         []string
	repo           string
	prNumber       string
}

type prInfo struct {
	Number            int                    `json:"number"`
	Title             string                 `json:"title"`
	HeadRefName       string                 `json:"headRefName"`
	Author            map[string]interface{} `json:"author"`
	StatusCheckRollup []checkStatus          `json:"statusCheckRollup"`
}

type checkStatus struct {
	Name       string `json:"name"`
	Conclusion string `json:"conclusion"`
}

type workflowRun struct {
	DatabaseID int    `json:"databaseId"`
	Name       string `json:"name"`
	Conclusion string `json:"conclusion"`
	Status     string `json:"status"`
	Event      string `json:"event"`
}

func runRestartFailed(out *output.Writer, opts restartFailedOpts) error {
	// Show what we're ignoring
	if len(opts.ignorePatterns) > 0 {
		out.Warning("Ignoring workflows matching: %s", strings.Join(opts.ignorePatterns, ", "))
	}

	// If specific PR is provided, restart it directly
	if opts.prNumber != "" {
		return restartSpecificPR(out, opts)
	}

	// Interactive mode: list and select PRs
	return restartInteractive(out, opts)
}

func restartSpecificPR(out *output.Writer, opts restartFailedOpts) error {
	out.Info("Fetching PR #%s...", opts.prNumber)

	// Build gh command
	args := []string{"pr", "view", opts.prNumber}
	if opts.repo != "" {
		args = append(args, "-R", opts.repo)
	}
	args = append(args, "--json", "number,title,headRefName,author")

	cmd := exec.Command("gh", args...)
	output, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("failed to fetch PR: %w", err)
	}

	var pr prInfo
	if err := json.Unmarshal(output, &pr); err != nil {
		return fmt.Errorf("failed to parse PR info: %w", err)
	}

	out.Success("PR #%d: %s", pr.Number, pr.Title)

	return restartPRWorkflows(out, opts, pr.Number, pr.HeadRefName)
}

func restartInteractive(out *output.Writer, opts restartFailedOpts) error {
	// Check if fzf is available
	if _, err := exec.LookPath("fzf"); err != nil {
		return fmt.Errorf("fzf is required but not found in PATH: %w", err)
	}

	out.Info("Fetching pull requests...")

	// Build gh pr list command
	args := []string{"pr", "list"}
	if opts.repo != "" {
		args = append(args, "-R", opts.repo)
	}
	for _, label := range opts.labels {
		args = append(args, "--label", label)
	}
	args = append(args, "--json", "number,title,headRefName,author,statusCheckRollup", "--limit", "100")

	cmd := exec.Command("gh", args...)
	output, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("failed to list PRs: %w", err)
	}

	var prs []prInfo
	if err := json.Unmarshal(output, &prs); err != nil {
		return fmt.Errorf("failed to parse PRs: %w", err)
	}

	// Filter PRs with failed checks
	type failedPRInfo struct {
		pr          prInfo
		failedCount int
		author      string
	}

	failedPRs := []failedPRInfo{}
	for _, pr := range prs {
		failedCount := 0
		for _, check := range pr.StatusCheckRollup {
			if check.Conclusion == "FAILURE" || check.Conclusion == "TIMED_OUT" ||
				check.Conclusion == "STARTUP_FAILURE" || check.Conclusion == "ACTION_REQUIRED" {
				failedCount++
			}
		}
		if failedCount > 0 {
			author := "unknown"
			if login, ok := pr.Author["login"].(string); ok {
				author = login
			}
			failedPRs = append(failedPRs, failedPRInfo{
				pr:          pr,
				failedCount: failedCount,
				author:      author,
			})
		}
	}

	if len(failedPRs) == 0 {
		out.Success("No pull requests with failed checks found!")
		return nil
	}

	out.Success("Found %d pull request(s) with failed checks", len(failedPRs))

	// Build fzf input with formatted PR information
	var fzfInput strings.Builder
	prMap := make(map[int]failedPRInfo) // Map PR number to info for later lookup

	for _, fpr := range failedPRs {
		prMap[fpr.pr.Number] = fpr
		// Format: "#123  Title here (@author) - 2 failed"
		fzfInput.WriteString(fmt.Sprintf("#%-6d %s (@%s) - %d failed\n",
			fpr.pr.Number, fpr.pr.Title, fpr.author, fpr.failedCount))
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
		       (.statusCheckRollup // [] | map(
		         "  " + (
		           if .conclusion == "SUCCESS" then "✓"
		           elif .conclusion == "FAILURE" then "✗"
		           elif .conclusion == "PENDING" then "●"
		           elif .conclusion == "SKIPPED" then "○"
		           else "?"
		           end
		         ) + " " + .name + " (" + (.conclusion // "unknown") + ")"
		       ) | join("\n"))'`, repoFlag)

	// Use fzf for multi-select with preview
	out.Info("Select pull requests to restart (use Tab to select multiple, Enter to confirm)...")
	fzfCmd := exec.Command("fzf",
		"--multi",
		"--ansi",
		"--header", "Select PRs to restart workflows (Tab: select, Enter: confirm)",
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

	// Restart workflows for selected PRs
	for _, prNum := range selectedPRNumbers {
		fpr, ok := prMap[prNum]
		if !ok {
			out.Warning("PR #%d not found in the list, skipping...", prNum)
			continue
		}

		out.Info("PR #%d: %s", fpr.pr.Number, fpr.pr.Title)
		if err := restartPRWorkflows(out, opts, fpr.pr.Number, fpr.pr.HeadRefName); err != nil {
			out.Error("Failed to restart workflows: %v", err)
		}
		out.Println("")
	}

	out.Success("Done!")
	return nil
}

func restartPRWorkflows(out *output.Writer, opts restartFailedOpts, prNumber int, branch string) error {
	// Get failed workflow runs for this PR
	args := []string{"run", "list", "--branch", branch}
	if opts.repo != "" {
		args = append(args, "-R", opts.repo)
	}
	args = append(args, "--json", "databaseId,name,conclusion,status,event", "--limit", "50")

	cmd := exec.Command("gh", args...)
	output, err := cmd.Output()
	if err != nil {
		return fmt.Errorf("failed to list workflow runs: %w", err)
	}

	var runs []workflowRun
	if err := json.Unmarshal(output, &runs); err != nil {
		return fmt.Errorf("failed to parse workflow runs: %w", err)
	}

	// Filter failed runs
	failedRuns := []workflowRun{}
	for _, run := range runs {
		// Check if it's a PR event and failed
		if run.Event != "pull_request" {
			continue
		}

		if run.Conclusion != "failure" && run.Conclusion != "timed_out" &&
			run.Conclusion != "startup_failure" && run.Conclusion != "action_required" {
			continue
		}

		// Check ignore patterns
		ignored := false
		for _, pattern := range opts.ignorePatterns {
			if strings.Contains(run.Name, pattern) {
				ignored = true
				break
			}
		}

		if !ignored {
			failedRuns = append(failedRuns, run)
		}
	}

	if len(failedRuns) == 0 {
		out.Warning("  No failed workflow runs found (may have been restarted already)")
		return nil
	}

	out.Info("  Restarting %d failed workflow(s):", len(failedRuns))

	// Restart each failed workflow
	for _, run := range failedRuns {
		out.Print("  → Restarting: %s (%s)... ", run.Name, run.Conclusion)

		rerunArgs := []string{"run", "rerun", fmt.Sprintf("%d", run.DatabaseID), "--failed"}
		if opts.repo != "" {
			rerunArgs = append(rerunArgs, "-R", opts.repo)
		}

		rerunCmd := exec.Command("gh", rerunArgs...)
		rerunOutput, err := rerunCmd.CombinedOutput()
		outputStr := strings.TrimSpace(string(rerunOutput))

		if err != nil || strings.Contains(outputStr, "error") {
			if strings.Contains(outputStr, "created over a month ago") {
				out.Warning("⚠ Cannot restart: workflow run is too old (>1 month)")
			} else {
				out.Error("✗ Failed: %s", outputStr)
			}
		} else {
			out.Success("✓")
		}
	}

	return nil
}
