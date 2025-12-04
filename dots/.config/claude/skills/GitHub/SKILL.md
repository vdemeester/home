---
name: GitHub
description: GitHub repository and pull request management. USE WHEN working with GitHub PRs, checking CI/CD status, reviewing pull requests, managing issues, or using gh CLI commands.
---

# GitHub

Expert guidance for GitHub workflows, pull request management, and repository operations using the gh CLI and custom tools.

## Purpose

This skill helps you efficiently manage GitHub repositories, pull requests, and CI/CD workflows using the gh command-line tool and custom automation.

### Context Detection

**This skill activates when:**
- User asks about GitHub pull requests or PR status
- User mentions gh CLI commands or GitHub workflows
- User wants to check CI/CD status, GitHub Actions, or PR checks
- User asks about reviewing PRs, managing issues, or GitHub operations
- Current directory is a git repository with GitHub remote

## Workflow Routing

**When executing a workflow, output this notification directly:**

```
Running the **WorkflowName** workflow from the **GitHub** skill...
```

| Workflow | Trigger | File |
|----------|---------|------|
| **CheckStatus** | "check pr status", "pr checks", "ci status", "check failures" | `workflows/CheckStatus.md` |
| **ReviewPR** | "review pr", "review pull request", "check pr changes" | `workflows/ReviewPR.md` |
| **CreatePR** | "create pr", "open pull request", "new pr" | `workflows/CreatePR.md` |
| **ManageIssues** | "create issue", "list issues", "close issue" | `workflows/ManageIssues.md` |
| **RestartChecks** | "restart checks", "rerun failed", "retry ci" | `workflows/RestartChecks.md` |

## Quick Reference

### Repository Context

When working with GitHub commands, you often need the repository owner/name. Here's how to get it:

```bash
# Get current repository owner/name using gh CLI (preferred)
gh repo view --json nameWithOwner -q .nameWithOwner
# Output: owner/repo

# Alternative: Parse from git remote URL
git remote get-url origin | sed -n 's#.*github\.com[:/]\(.*\)\.git#\1#p'
# Output: owner/repo

# Get just the owner
gh repo view --json owner -q .owner.login

# Get just the repo name
gh repo view --json name -q .name
```

**Usage in workflows:**
```bash
# Store for multiple commands
REPO=$(gh repo view --json nameWithOwner -q .nameWithOwner)
gh pr list -R "$REPO"
gh run list -R "$REPO"

# Or use directly in commands
gh pr checks -R "$(gh repo view --json nameWithOwner -q .nameWithOwner)" 123
```

### Essential gh Commands

```bash
# PR operations
gh pr list                          # List PRs
gh pr status                        # Show status of relevant PRs
gh pr view [number]                 # View PR details
gh pr checks [number]               # Show check status
gh pr create                        # Create new PR
gh pr review [number]               # Review a PR
gh pr merge [number]                # Merge a PR

# Issue operations
gh issue list                       # List issues
gh issue view [number]              # View issue details
gh issue create                     # Create new issue
gh issue close [number]             # Close an issue

# Repository operations
gh repo view                        # View repository
gh repo clone <repo>                # Clone repository
gh run list                         # List workflow runs
gh run view [id]                    # View run details
gh run rerun [id]                   # Rerun a workflow
```

### Check Status Quick View

```bash
# Check PR status with failures
gh pr checks --watch

# View specific PR checks
gh pr checks 123

# List failed workflow runs
gh run list --workflow=CI --status=failure

# View workflow run logs
gh run view <run-id> --log
```

## Common Workflows

### Check PR Status
See all checks for a PR, identify failures, and get actionable information:
```bash
gh pr checks 123
gh pr view 123 --json statusCheckRollup
```

### Review a PR
Get PR details, view diff, and leave review comments:
```bash
gh pr view 123
gh pr diff 123
gh pr review 123 --approve
gh pr review 123 --request-changes --body "Feedback here"
```

### Restart Failed Checks
Rerun failed CI checks for a PR:
```bash
# Get failed run IDs
gh run list --branch=pr-branch --status=failure

# Rerun specific run
gh run rerun <run-id>

# Rerun failed jobs only
gh run rerun <run-id> --failed
```

## Tools

### Available: gh-restart-failed

**Location**: `~/src/home/tools/gh-restart-failed/`

Interactive tool to list and restart failed GitHub workflow checks on pull requests.

**Features:**
- Lists all PRs with failed checks
- Interactive selection with fzf (multi-select supported)
- Preview failed checks before restarting
- Ignore specific workflows by pattern
- Filter PRs by label
- Color-coded output

**Usage:**
```bash
# Use current repository
gh-restart-failed

# Use specific repository
gh-restart-failed owner/repo

# Use a pull request in a specific repository
gh-restart-failed owner/repo#123

# Ignore specific workflows
gh-restart-failed -i "Label Checker" -i "build"

# Filter by PR labels
gh-restart-failed -l "bug" -l "enhancement"

# Show help
gh-restart-failed --help
```

**Dependencies**: gh, fzf, jq (automatically provided by Nix package)

**How it works:**
1. Fetches all open PRs with their check status
2. Filters PRs with failed checks (FAILURE, TIMED_OUT, STARTUP_FAILURE, ACTION_REQUIRED)
3. Shows interactive fzf picker with preview of failed checks
4. Restarts only failed jobs for selected PRs
5. Handles edge cases (old runs, already restarted, etc.)

### Custom Tools (to be implemented)

Custom tools in `tools/` directory for additional behaviors:

- **CheckPRStatus.sh** - Enhanced PR check status with filtering and formatting
- **PRSummary.py** - Generate comprehensive PR summaries

(See `tools/README.md` for development guidelines)

## Integration with Git Skill

This skill works alongside the Git skill:
- **Git skill**: Local repository operations (commits, branches, rebasing)
- **GitHub skill**: Remote GitHub operations (PRs, issues, checks)

When both apply, prefer:
- Git skill for: commits, branches, local workflows
- GitHub skill for: PRs, CI/CD, issues, reviews

## Best Practices

1. **Use gh CLI for standard operations**: It's faster and more reliable than web UI
2. **Check PR status before merge**: Always verify checks pass
3. **Review locally when possible**: Use `gh pr checkout` to test changes
4. **Automate repetitive tasks**: Use custom tools for complex workflows
5. **Monitor CI/CD actively**: Don't wait for email notifications

## Examples

**Example 1: Check PR status**
```
User: "Check the status of PR #123"
→ Invokes CheckStatus workflow
→ Shows all checks with pass/fail status
→ Highlights any failures with logs
→ Provides actionable next steps
```

**Example 2: Review and merge PR**
```
User: "Review PR #456 and merge if tests pass"
→ Invokes ReviewPR workflow
→ Shows PR details and diff
→ Checks all CI status
→ Approves and merges if all green
```

**Example 3: Restart failed checks**
```
User: "Restart the failed checks on my PR"
→ Invokes RestartChecks workflow
→ Identifies current PR branch
→ Finds failed workflow runs
→ Reruns only failed jobs
```

## Resources

- [GitHub CLI Manual](https://cli.github.com/manual/)
- [gh pr documentation](https://cli.github.com/manual/gh_pr)
- [gh run documentation](https://cli.github.com/manual/gh_run)
- [GitHub Actions documentation](https://docs.github.com/en/actions)
