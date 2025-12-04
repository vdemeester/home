# GitHub Skill Tools

Custom tools for enhanced GitHub workflow automation.

## Purpose

While `gh` CLI handles most GitHub operations, these tools provide enhanced functionality for:
- Complex PR status formatting and filtering
- Intelligent check restart logic
- Automated PR template filling
- Advanced workflow automation

## Available Tools

### Planned Tools

**CheckPRStatus.sh**
- Enhanced PR check status with color-coded output
- Automatic log fetching for failures
- Smart filtering (required checks, failed only, pending)
- Export to JSON/Markdown
- Notification integration

**RestartFailedChecks.sh**
- Detect flaky vs. code failures from logs
- Automatic retry for flaky tests (with configurable limits)
- Skip restart if same failure occurred multiple times
- Statistics tracking (success rate, retry count)
- Slack/email notifications

**CreatePR.sh**
- Auto-detect and parse PR templates
- Intelligently fill template sections based on commits
- Auto-check completed checklist items
- Detect PR type from branch name or commits
- Suggest reviewers based on file changes (git blame)
- Validate PR against project guidelines

**PRSummary.py**
- Generate comprehensive PR summaries
- Analyze code changes and generate description
- Extract key changes and impact
- Suggest labels and reviewers
- Integration with AI for smart summaries (complex logic warrants Python)

## Tool Development Guidelines

### Technology Stack

- **Bash/Shell**: Primary choice for most tools (simple, portable, direct `gh` integration)
- **Python with uv**: For complex logic requiring data structures or libraries
- **nix-shell**: For shell scripts needing specific dependencies

### Structure

**Shell Script Example:**

```bash
#!/usr/bin/env nix-shell
#! nix-shell -i bash -p gh jq

# CheckPRStatus.sh - Enhanced PR check status viewer

set -euo pipefail

# Parse arguments
PR_NUMBER="${1:-}"
FILTER="${2:-all}"  # all, failed, pending, required

if [ -z "$PR_NUMBER" ]; then
  echo "Usage: $0 <pr-number> [filter]"
  exit 1
fi

# Get PR checks
CHECKS=$(gh pr view "$PR_NUMBER" --json statusCheckRollup --jq '.statusCheckRollup')

# Filter and format
case "$FILTER" in
  failed)
    echo "$CHECKS" | jq -r '.[] | select(.conclusion == "failure") | "✗ \(.name)"'
    ;;
  pending)
    echo "$CHECKS" | jq -r '.[] | select(.status != "COMPLETED") | "⏳ \(.name)"'
    ;;
  required)
    echo "$CHECKS" | jq -r '.[] | select(.isRequired == true) | "\(.conclusion // .status) \(.name)"'
    ;;
  *)
    echo "$CHECKS" | jq -r '.[] | "\(.conclusion // .status) \(.name)"'
    ;;
esac
```

**Python Script Example (for complex logic):**

```python
#!/usr/bin/env -S uv run --quiet --script
# /// script
# dependencies = ["click", "requests"]
# ///

"""
PRSummary.py - Generate intelligent PR summaries
"""

import click
import json
import subprocess

@click.command()
@click.option('--pr', required=True, help='PR number')
@click.option('--format', default='markdown', help='Output format')
def summarize_pr(pr: int, format: str):
    """Generate comprehensive PR summary"""

    # Get PR data via gh
    result = subprocess.run(
        ['gh', 'pr', 'view', str(pr), '--json', 'title,body,commits,files'],
        capture_output=True,
        text=True
    )

    data = json.loads(result.stdout)

    # Analyze commits
    commits = data['commits']
    files_changed = len(data['files'])

    # Generate summary
    summary = f"""
# PR #{pr}: {data['title']}

## Changes
- {files_changed} files changed
- {len(commits)} commits

## Commit Summary
"""

    for commit in commits:
        summary += f"- {commit['messageHeadline']}\n"

    click.echo(summary)

if __name__ == '__main__':
    summarize_pr()
```

### Best Practices

1. **Prefer shell scripts**: Start with bash, only use Python if truly needed
2. **Use nix-shell shebang**: For scripts with dependencies (jq, curl, etc.)
3. **Use uv for Python**: Inline dependency management with `# /// script`
4. **Fail fast**: Exit early with clear error messages (`set -euo pipefail`)
5. **Validate inputs**: Check for required options and valid values
6. **Provide feedback**: Show progress for long-running operations
7. **Handle errors gracefully**: Don't crash, provide actionable error messages
8. **Make it scriptable**: Support JSON output for automation
9. **Add help text**: Include usage message when called incorrectly
10. **Follow conventions**: Use standard flags like `-v`, `--json`, `--dry-run`

### Dependencies

**Shell Scripts:**
```bash
#!/usr/bin/env nix-shell
#! nix-shell -i bash -p gh jq curl fzf

# Script has access to: gh, jq, curl, fzf
```

**Python Scripts:**
```python
#!/usr/bin/env -S uv run --quiet --script
# /// script
# dependencies = [
#   "click>=8.0",
#   "requests>=2.28",
# ]
# ///
```

### Example: CheckPRStatus.sh

```bash
#!/usr/bin/env nix-shell
#! nix-shell -i bash -p gh jq

# CheckPRStatus.sh - Enhanced PR check status viewer
# Usage: ./CheckPRStatus.sh <pr-number> [--filter failed|pending|required]

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Parse arguments
PR_NUMBER="${1:-}"
FILTER_TYPE="${2:-all}"

show_usage() {
  cat << EOF
Usage: $0 <pr-number> [--filter <type>]

Arguments:
  pr-number      PR number to check

Options:
  --filter TYPE  Filter checks (all|failed|pending|required)
  --show-logs    Show failure logs
  --json         Output as JSON

Examples:
  $0 123                    # Show all checks
  $0 123 --filter failed    # Show only failed checks
  $0 123 --show-logs        # Show failures with logs
EOF
}

if [ -z "$PR_NUMBER" ]; then
  show_usage
  exit 1
fi

# Get PR checks
CHECKS_JSON=$(gh pr view "$PR_NUMBER" --json statusCheckRollup --jq '.statusCheckRollup')

# Count statuses
TOTAL=$(echo "$CHECKS_JSON" | jq 'length')
PASSED=$(echo "$CHECKS_JSON" | jq '[.[] | select(.conclusion == "success")] | length')
FAILED=$(echo "$CHECKS_JSON" | jq '[.[] | select(.conclusion == "failure")] | length')
PENDING=$(echo "$CHECKS_JSON" | jq '[.[] | select(.status != "COMPLETED")] | length')

# Show summary
echo -e "\nPR #$PR_NUMBER Check Status"
echo "=============================="
echo -e "Total: $TOTAL | ${GREEN}Passed: $PASSED${NC} | ${RED}Failed: $FAILED${NC} | ${YELLOW}Pending: $PENDING${NC}\n"

# Filter and display
case "${FILTER_TYPE}" in
  --filter)
    case "${3:-all}" in
      failed)
        echo -e "${RED}Failed Checks:${NC}"
        echo "$CHECKS_JSON" | jq -r '.[] | select(.conclusion == "failure") | "  ✗ \(.name)\n    → \(.detailsUrl)"'
        ;;
      pending)
        echo -e "${YELLOW}Pending Checks:${NC}"
        echo "$CHECKS_JSON" | jq -r '.[] | select(.status != "COMPLETED") | "  ⏳ \(.name)"'
        ;;
      required)
        echo -e "Required Checks:"
        echo "$CHECKS_JSON" | jq -r '.[] | select(.isRequired == true) | "  \(if .conclusion == "success" then "✓" elif .conclusion == "failure" then "✗" else "⏳" end) \(.name)"'
        ;;
      all|*)
        echo "$CHECKS_JSON" | jq -r '.[] | "\(if .conclusion == "success" then "✓" elif .conclusion == "failure" then "✗" else "⏳" end) \(.name)"'
        ;;
    esac
    ;;
  --show-logs)
    echo -e "${RED}Failed Checks with Logs:${NC}\n"

    # Get failed run IDs
    BRANCH=$(gh pr view "$PR_NUMBER" --json headRefName --jq '.headRefName')
    FAILED_RUNS=$(gh run list --branch="$BRANCH" --status=failure --json databaseId --jq '.[].databaseId' | head -3)

    for run_id in $FAILED_RUNS; do
      echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
      gh run view "$run_id" --log-failed | head -50
      echo ""
    done
    ;;
  --json)
    echo "$CHECKS_JSON" | jq '.'
    ;;
  *)
    # Default: show all
    echo "$CHECKS_JSON" | jq -r '.[] | "\(if .conclusion == "success" then "✓" elif .conclusion == "failure" then "✗" else "⏳" end) \(.name)"'
    ;;
esac
```

### Testing Tools

```bash
# Make executable
chmod +x tools/CheckPRStatus.sh

# Test (nix-shell will automatically provide dependencies)
./tools/CheckPRStatus.sh 123

# Test with filter
./tools/CheckPRStatus.sh 123 --filter failed

# Test with logs
./tools/CheckPRStatus.sh 123 --show-logs
```

## Integration with Workflows

Tools are referenced in workflow files:

```markdown
## Custom Tool: Enhanced Status Check

For complex formatting:

\`\`\`bash
./tools/CheckPRStatus.sh 123 --filter failed --show-logs
\`\`\`
```

## Development Workflow

1. **Plan the tool**: Define what it should do
2. **Check if gh can do it**: Don't duplicate existing functionality
3. **Choose language**: Shell for simple, Python for complex
4. **Add dependencies**: Use nix-shell or uv shebang
5. **Prototype**: Write minimal version
6. **Test**: Test with real PRs/repos
7. **Make executable**: `chmod +x tools/ToolName.sh`
8. **Document**: Add usage examples to workflow files
9. **Integrate**: Reference in appropriate workflows

## Future Tools

- **PRMetrics.sh**: Analyze PR size, complexity, review time
- **AutoLabel.sh**: Automatically suggest labels based on file changes
- **ReviewReminder.sh**: Notify reviewers of pending PRs
- **MergeQueue.sh**: Manage PR merge order with dependencies
- **ChangelogGenerator.sh**: Generate changelog from merged PRs

## Contributing

When adding a new tool:

1. Create script in `tools/` (prefer `.sh`, use `.py` only if needed)
2. Add appropriate shebang (nix-shell or uv)
3. Add usage/help message
4. Make executable: `chmod +x`
5. Document in this README
6. Reference in relevant workflow files
7. Add usage examples
8. Test with real GitHub data
