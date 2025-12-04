# CheckStatus Workflow

Check the status of GitHub PR checks, identify failures, and provide actionable information.

## When to Use

- "check pr status"
- "show pr checks"
- "ci status"
- "check failures"
- "are the checks passing?"

## Quick Commands

### Check Current PR
```bash
# Auto-detect current branch's PR and show checks
gh pr checks

# Watch checks in real-time
gh pr checks --watch

# Show checks for specific PR
gh pr checks 123
```

### Detailed Status
```bash
# Get full status including URLs and conclusions
gh pr view 123 --json statusCheckRollup --jq '.statusCheckRollup'

# List all workflow runs for current branch
gh run list --branch=$(git branch --show-current)

# Show failed runs only
gh run list --branch=$(git branch --show-current) --status=failure
```

## Workflow Steps

### 1. Identify the PR

**If PR number is provided:**
```bash
gh pr view <number>
```

**If no PR number (auto-detect):**
```bash
# Get PR for current branch
gh pr status --json currentBranch,state,number --jq '.currentBranch.number'
```

### 2. Get Check Status

```bash
# Simple check list
gh pr checks <number>

# Detailed JSON output
gh pr view <number> --json statusCheckRollup,state,mergeable
```

### 3. Analyze Results

Parse the output to identify:
- ✓ **Passing checks**: All green
- ✗ **Failed checks**: What failed and why
- ⏳ **Pending checks**: Still running
- ⊘ **Skipped checks**: Not required

### 4. Show Failure Details

For each failed check:
```bash
# Get the workflow run ID
gh run list --workflow="Check Name" --branch=branch-name --limit=1 --json databaseId --jq '.[0].databaseId'

# View the run with logs
gh run view <run-id> --log-failed

# Or view in browser
gh run view <run-id> --web
```

### 5. Present Summary

**Format:**
```
PR #123: feature-branch → main

Status: ❌ Checks failing (2/5)

✓ tests (2m 34s)
✓ lint (1m 12s)
✓ build (3m 45s)
✗ e2e-tests (4m 23s) - FAILED
✗ security-scan (2m 01s) - FAILED

Failed Checks:

1. e2e-tests
   Run: https://github.com/owner/repo/actions/runs/123456
   Error: Test suite "auth" failed with 2 failures

   Next steps:
   - View logs: gh run view 123456 --log-failed
   - Rerun: gh run rerun 123456 --failed

2. security-scan
   Run: https://github.com/owner/repo/actions/runs/123457
   Error: Found 1 high severity vulnerability

   Next steps:
   - View details: gh run view 123457 --log
   - Fix and push changes
```

## Advanced Usage

### Check Multiple PRs

```bash
# List all open PRs with check status
gh pr list --json number,title,statusCheckRollup --jq '.[] | "\(.number): \(.title) - \(.statusCheckRollup | length) checks"'
```

### Monitor Checks

```bash
# Watch checks until completion
gh pr checks --watch --interval 30
```

### Filter by Status

```bash
# Show only failed checks
gh pr view <number> --json statusCheckRollup --jq '.statusCheckRollup[] | select(.conclusion == "failure")'

# Show only required checks
gh pr view <number> --json statusCheckRollup --jq '.statusCheckRollup[] | select(.isRequired == true)'
```

## Custom Tool: Enhanced Status Check

For more complex formatting and filtering, use the custom tool:

```bash
# In tools/ directory
./CheckPRStatus.ts --pr 123 --show-logs --filter failed
```

**Tool capabilities:**
- Color-coded output
- Automatic log fetching for failures
- Smart filtering (required, failed, pending)
- Export to JSON/Markdown
- Integration with notifications

## Common Scenarios

### Scenario 1: Quick Status Check
```bash
# Just want to know if checks passed
gh pr checks && echo "✓ All checks passed" || echo "✗ Some checks failed"
```

### Scenario 2: Debug Specific Failure
```bash
# Get PR number
PR=$(gh pr status --json currentBranch --jq '.currentBranch.number')

# Find failed runs
gh run list --branch=$(git branch --show-current) --status=failure --limit=1 --json databaseId --jq '.[0].databaseId'

# View failure logs
gh run view <run-id> --log-failed
```

### Scenario 3: Wait for Checks to Complete
```bash
# Watch until all complete
gh pr checks --watch

# Or poll in script
while true; do
  STATUS=$(gh pr view <number> --json statusCheckRollup --jq '.statusCheckRollup | map(select(.status != "COMPLETED")) | length')
  if [ "$STATUS" -eq 0 ]; then
    echo "All checks completed"
    break
  fi
  echo "Waiting for $STATUS checks..."
  sleep 30
done
```

## Error Handling

**No PR found:**
```
Error: Current branch has no associated PR
→ Create one: gh pr create
```

**PR is draft:**
```
PR #123 is a draft
Checks may not run until marked as ready for review
→ Mark ready: gh pr ready 123
```

**No checks configured:**
```
No status checks found for this PR
→ Check if GitHub Actions are configured
→ Verify branch protection rules
```

## Integration with Other Workflows

- **RestartChecks**: If checks failed, offer to restart
- **ReviewPR**: Show check status as part of review
- **CreatePR**: Check status immediately after creation

## Best Practices

1. **Check before requesting review**: Ensure checks pass before asking for review
2. **Monitor long-running checks**: Use `--watch` for jobs over 5 minutes
3. **Investigate failures immediately**: Don't wait for multiple failures
4. **Use JSON output for automation**: Parse with `jq` for scripts
5. **Keep check names descriptive**: Makes debugging easier

## Output Formats

### Simple (default)
```
gh pr checks 123
```

### JSON (for scripts)
```bash
gh pr view 123 --json statusCheckRollup
```

### Custom (with jq)
```bash
gh pr view 123 --json statusCheckRollup --jq '
  .statusCheckRollup |
  group_by(.conclusion) |
  map({conclusion: .[0].conclusion, count: length})
'
```

## Resources

- [gh pr checks documentation](https://cli.github.com/manual/gh_pr_checks)
- [gh run list documentation](https://cli.github.com/manual/gh_run_list)
- [GitHub Checks API](https://docs.github.com/en/rest/checks)
