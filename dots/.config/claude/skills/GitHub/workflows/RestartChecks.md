# RestartChecks Workflow

Intelligently restart failed GitHub workflow checks for a pull request.

## When to Use

- "restart checks"
- "rerun failed checks"
- "retry ci"
- "restart failed jobs"
- "rerun workflow"

## Quick Commands

### Restart Failed Jobs

```bash
# Rerun only failed jobs from last run
gh run rerun <run-id> --failed

# Rerun entire workflow
gh run rerun <run-id>

# Rerun specific job
gh run rerun <run-id> --job <job-id>
```

### Find Failed Runs

```bash
# List failed runs for current branch
gh run list --branch=$(git branch --show-current) --status=failure

# Get latest failed run ID
gh run list --branch=$(git branch --show-current) --status=failure --limit=1 --json databaseId --jq '.[0].databaseId'
```

## Workflow Steps

### 1. Identify Target PR/Branch

**If PR number provided:**
```bash
# Get branch from PR
BRANCH=$(gh pr view <number> --json headRefName --jq '.headRefName')
```

**If no PR (use current branch):**
```bash
BRANCH=$(git branch --show-current)
```

### 2. Find Failed Runs

```bash
# Get all failed runs for the branch
gh run list --branch="$BRANCH" --status=failure --json databaseId,name,conclusion,createdAt

# Group by workflow name to get latest failed run for each
gh run list --branch="$BRANCH" --status=failure --json databaseId,name,workflowName --jq '
  group_by(.workflowName) |
  map({workflow: .[0].workflowName, runId: .[0].databaseId})
'
```

### 3. Present Options

Show the user which checks failed:

```
Failed checks on branch: feature-branch

1. CI Tests (run: 123456)
   - Failed 2 minutes ago
   - 3 jobs failed

2. Security Scan (run: 123457)
   - Failed 5 minutes ago
   - 1 job failed

3. Build (run: 123458)
   - Failed 1 minute ago
   - 2 jobs failed

Restart options:
a) All failed checks
b) Specific check (choose number)
c) Cancel
```

### 4. Execute Restart

**Restart all failed:**
```bash
for run_id in $(gh run list --branch="$BRANCH" --status=failure --json databaseId --jq '.[].databaseId'); do
  gh run rerun "$run_id" --failed
  echo "Restarted run: $run_id"
done
```

**Restart specific:**
```bash
gh run rerun <run-id> --failed
```

### 5. Monitor Progress

```bash
# Watch the restarted checks
gh run watch <run-id>

# Or show PR checks
gh pr checks --watch
```

### 6. Confirm Completion

```
✓ Restarted 3 failed workflow runs

Monitoring:
- CI Tests: https://github.com/owner/repo/actions/runs/123459
- Security Scan: https://github.com/owner/repo/actions/runs/123460
- Build: https://github.com/owner/repo/actions/runs/123461

Watch progress: gh pr checks --watch
```

## Advanced Usage

### Restart with Different Parameters

Some workflows accept inputs for reruns:

```bash
# Rerun with specific inputs
gh workflow run <workflow-name> --ref <branch> -f param=value
```

### Smart Restart (Skip Non-Flaky Failures)

If failures are due to code issues (not flaky tests), ask user first:

```bash
# Analyze failure logs
gh run view <run-id> --log-failed

# If failure looks like code issue, suggest fixing first
echo "Failure appears to be code-related. Fix the issue before restarting?"
```

### Restart in Batch

For multiple PRs or branches:

```bash
# Get all PRs with failed checks
gh pr list --json number,headRefName,statusCheckRollup --jq '
  .[] |
  select(.statusCheckRollup | any(.conclusion == "failure")) |
  .number
'

# Restart each
for pr in $(gh pr list ...); do
  BRANCH=$(gh pr view "$pr" --json headRefName --jq '.headRefName')
  # Restart failed runs...
done
```

## Common Scenarios

### Scenario 1: Quick Restart Current PR

```bash
# Get current PR
PR=$(gh pr status --json currentBranch --jq '.currentBranch.number')

# Find and restart failed runs
FAILED_RUNS=$(gh run list --branch=$(git branch --show-current) --status=failure --json databaseId --jq '.[].databaseId')

# Restart each
for run in $FAILED_RUNS; do
  gh run rerun "$run" --failed
done

# Watch
gh pr checks --watch
```

### Scenario 2: Restart Specific Failed Job

```bash
# View run to see which jobs failed
gh run view <run-id>

# Get failed job IDs
gh run view <run-id> --json jobs --jq '.jobs[] | select(.conclusion == "failure") | .databaseId'

# Restart specific job
gh run rerun <run-id> --job <job-id>
```

### Scenario 3: Automatic Retry on Flaky Tests

```bash
# Check if failure is flaky (appears in logs)
if gh run view <run-id> --log-failed | grep -q "flaky\|timeout\|network"; then
  echo "Detected flaky failure, restarting automatically..."
  gh run rerun <run-id> --failed
else
  echo "Failure appears to be code-related. Review before restarting."
fi
```

## Available Tool: gh-restart-failed

**Interactive tool for restarting failed checks across multiple PRs:**

```bash
# List and restart failed checks interactively
gh-restart-failed

# Use specific repository
gh-restart-failed owner/repo

# Ignore specific workflows (Label Checker ignored by default)
gh-restart-failed -i "build" -i "test"

# Filter by PR labels
gh-restart-failed -l "bug" -l "enhancement"
```

**Features:**
- Interactive fzf interface with multi-select
- Preview failed checks before restarting
- Handles multiple PRs at once
- Filters out old runs (>1 month)
- Color-coded output with status

**Location**: `~/src/home/tools/gh-restart-failed/`

## Custom Tool: Intelligent Restart (Planned)

For more sophisticated restart logic:

```bash
# In tools/ directory
./RestartFailedChecks.sh --pr 123 --auto-flaky --max-retries 2
```

**Planned capabilities:**
- Detect flaky vs. code failures
- Automatic retry for flaky tests (with limits)
- Skip restart if same failure occurred multiple times
- Notify on Slack/email when checks complete
- Statistics tracking (success rate, retry count)

## Error Handling

**No failed runs found:**
```
No failed checks found for this branch
→ All checks passing or none configured
```

**Workflow run not found:**
```
Error: Run ID not found
→ May have been deleted or expired
→ Check: gh run list --branch <branch>
```

**Permission denied:**
```
Error: Permission denied to rerun workflow
→ Requires write access to repository
→ Check: gh auth status
```

**Workflow already running:**
```
Cannot restart: workflow is already running
→ Wait for completion or cancel first
→ Cancel: gh run cancel <run-id>
```

## Best Practices

1. **Check logs before restarting**: Don't blindly restart failures
2. **Limit automatic retries**: Avoid infinite retry loops (max 2-3)
3. **Restart failed jobs only**: Use `--failed` to save CI resources
4. **Monitor after restart**: Ensure the retry succeeds
5. **Fix code issues first**: Don't restart if failure is code-related
6. **Track retry history**: Know when tests are consistently flaky

## Integration with Other Workflows

- **CheckStatus**: Identify failures before restarting
- **ReviewPR**: Restart as part of PR review process
- **ManageIssues**: Create issue for persistent failures

## Restart Strategies

### Conservative (Recommended)
```bash
# 1. Check what failed
gh run view <run-id> --log-failed

# 2. Confirm with user
echo "Restart failed jobs? (y/n)"

# 3. Restart only failed
gh run rerun <run-id> --failed
```

### Aggressive (For Flaky Tests)
```bash
# Auto-restart up to 3 times
RETRIES=0
MAX_RETRIES=3

while [ $RETRIES -lt $MAX_RETRIES ]; do
  gh run rerun <run-id> --failed

  # Wait for completion
  gh run watch <run-id>

  # Check if passed
  if gh run view <run-id> --json conclusion --jq '.conclusion' | grep -q "success"; then
    break
  fi

  RETRIES=$((RETRIES + 1))
done
```

### Selective (By Workflow)
```bash
# Only restart specific workflows (e.g., tests but not linting)
RESTARTABLE_WORKFLOWS=("CI Tests" "E2E Tests")

for workflow in "${RESTARTABLE_WORKFLOWS[@]}"; do
  RUN_ID=$(gh run list --workflow="$workflow" --branch="$BRANCH" --status=failure --limit=1 --json databaseId --jq '.[0].databaseId')
  if [ -n "$RUN_ID" ]; then
    gh run rerun "$RUN_ID" --failed
  fi
done
```

## Resources

- [gh run rerun documentation](https://cli.github.com/manual/gh_run_rerun)
- [gh run watch documentation](https://cli.github.com/manual/gh_run_watch)
- [GitHub Actions re-running workflows](https://docs.github.com/en/actions/managing-workflow-runs/re-running-workflows-and-jobs)
