package main

import (
	"encoding/json"
	"fmt"
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
	failedPRs := []prInfo{}
	for _, pr := range prs {
		hasFailed := false
		for _, check := range pr.StatusCheckRollup {
			if check.Conclusion == "FAILURE" || check.Conclusion == "TIMED_OUT" ||
				check.Conclusion == "STARTUP_FAILURE" || check.Conclusion == "ACTION_REQUIRED" {
				hasFailed = true
				break
			}
		}
		if hasFailed {
			failedPRs = append(failedPRs, pr)
		}
	}

	if len(failedPRs) == 0 {
		out.Success("No pull requests with failed checks found!")
		return nil
	}

	out.Warning("Found %d pull request(s) with failed checks:", len(failedPRs))
	out.Println("")

	// Display PRs for user
	for i, pr := range failedPRs {
		failedCount := 0
		for _, check := range pr.StatusCheckRollup {
			if check.Conclusion == "FAILURE" || check.Conclusion == "TIMED_OUT" ||
				check.Conclusion == "STARTUP_FAILURE" || check.Conclusion == "ACTION_REQUIRED" {
				failedCount++
			}
		}

		author := "unknown"
		if login, ok := pr.Author["login"].(string); ok {
			author = login
		}

		out.Println("%d. PR #%d: %s (@%s) - %d failed", i+1, pr.Number, pr.Title, author, failedCount)
	}

	out.Println("")
	out.Info("Processing all PRs with failed workflows...")
	out.Println("")

	// Restart workflows for each PR
	for _, pr := range failedPRs {
		out.Info("PR #%d: %s", pr.Number, pr.Title)
		if err := restartPRWorkflows(out, opts, pr.Number, pr.HeadRefName); err != nil {
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
