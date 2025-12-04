#!/usr/bin/env bash

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Help message
usage() {
    cat <<EOF
Usage: gh-restart-failed [OPTIONS] [REPOSITORY[#PR_NUMBER]]

List pull requests with failed checks and restart selected workflows.

Options:
    -i, --ignore PATTERN    Ignore workflows matching PATTERN (can be used multiple times)
    -l, --label LABEL       Filter PRs by label (can be used multiple times)
    -h, --help             Show this help message

Arguments:
    REPOSITORY    Optional repository in OWNER/REPO format or path to local repo.
                  If not provided, uses the current directory's repository.
                  Can include #PR_NUMBER to directly restart a specific PR (skips interactive selection).

Dependencies:
    - gh (GitHub CLI)
    - fzf (fuzzy finder, only needed for interactive mode)
    - jq (JSON processor)

Note:
    By default, "Label Checker" workflows are ignored. Use -i to add more patterns.

Examples:
    gh-restart-failed                                    # Use current repository (interactive)
    gh-restart-failed owner/repo#123                     # Directly restart PR #123 in owner/repo
    gh-restart-failed owner/repo                         # Use specific GitHub repository (interactive)
    gh-restart-failed -i "build" -i "test"              # Ignore build and test workflows
    gh-restart-failed -l "bug" -l "enhancement"         # Only show PRs with bug OR enhancement labels
    gh-restart-failed /path/to/repo                     # Use repository at path

EOF
    exit 0
}

# Check dependencies
check_dependencies() {
    local missing=()

    for cmd in gh jq; do
        if ! command -v "$cmd" &> /dev/null; then
            missing+=("$cmd")
        fi
    done

    if [ ${#missing[@]} -gt 0 ]; then
        echo -e "${RED}Error: Missing required dependencies: ${missing[*]}${NC}" >&2
        echo "Please install them and try again." >&2
        exit 1
    fi
}

# Default ignore patterns
IGNORE_PATTERNS=("Label Checker")
LABEL_FILTERS=()

# Parse arguments
REPO_ARG=""
PR_NUMBER=""
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -i|--ignore)
            if [ -n "${2:-}" ]; then
                IGNORE_PATTERNS+=("$2")
                shift 2
            else
                echo -e "${RED}Error: --ignore requires a pattern argument${NC}" >&2
                exit 1
            fi
            ;;
        -l|--label)
            if [ -n "${2:-}" ]; then
                LABEL_FILTERS+=("$2")
                shift 2
            else
                echo -e "${RED}Error: --label requires a label argument${NC}" >&2
                exit 1
            fi
            ;;
        -*)
            echo -e "${RED}Error: Unknown option: $1${NC}" >&2
            usage
            ;;
        *)
            REPO_ARG="$1"
            # Check if it contains #PR_NUMBER
            if [[ "$REPO_ARG" =~ ^(.+)#([0-9]+)$ ]]; then
                REPO_ARG="${BASH_REMATCH[1]}"
                PR_NUMBER="${BASH_REMATCH[2]}"
            fi
            shift
            ;;
    esac
done

check_dependencies

# Check fzf only if in interactive mode (no PR_NUMBER specified)
if [ -z "$PR_NUMBER" ] && ! command -v fzf &> /dev/null; then
    echo -e "${RED}Error: fzf is required for interactive mode${NC}" >&2
    echo "Please install fzf or specify a PR number directly (e.g., owner/repo#123)" >&2
    exit 1
fi

# Determine repository context
REPO_FLAG=()
if [ -n "$REPO_ARG" ]; then
    if [ -d "$REPO_ARG" ]; then
        # It's a directory path
        REPO_FLAG=(-R "$(cd "$REPO_ARG" && gh repo view --json nameWithOwner -q .nameWithOwner)")
    else
        # Assume it's OWNER/REPO format
        REPO_FLAG=(-R "$REPO_ARG")
    fi
fi

# Show ignored patterns
if [ ${#IGNORE_PATTERNS[@]} -gt 0 ]; then
    echo -e "${YELLOW}Ignoring workflows matching: ${IGNORE_PATTERNS[*]}${NC}" >&2
fi

# Show label filters
if [ ${#LABEL_FILTERS[@]} -gt 0 ]; then
    echo -e "${YELLOW}Filtering PRs with labels: ${LABEL_FILTERS[*]}${NC}" >&2
fi

# If PR_NUMBER is specified, skip interactive selection
if [ -n "$PR_NUMBER" ]; then
    echo -e "${BLUE}Fetching PR #$PR_NUMBER...${NC}" >&2

    # Fetch specific PR information
    pr_info=$(gh pr view "${REPO_FLAG[@]}" "$PR_NUMBER" \
        --json number,title,headRefName,author \
        2>/dev/null)

    if [ -z "$pr_info" ]; then
        echo -e "${RED}Error: PR #$PR_NUMBER not found${NC}" >&2
        exit 1
    fi

    pr_title=$(echo "$pr_info" | jq -r '.title')
    pr_branch=$(echo "$pr_info" | jq -r '.headRefName')
    pr_author=$(echo "$pr_info" | jq -r '.author.login')

    # Format as if selected from interactive mode
    selected_prs="#$PR_NUMBER | $pr_title | @$pr_author | $pr_branch | direct"
else
    # Interactive mode: Get all open PRs with their check status
    echo -e "${BLUE}Fetching pull requests...${NC}" >&2

    # Build label filter arguments for gh pr list
    LABEL_ARGS=()
    for label in "${LABEL_FILTERS[@]}"; do
        LABEL_ARGS+=(--label "$label")
    done

    # Fetch PRs with detailed check information
    prs_json=$(gh pr list "${REPO_FLAG[@]}" \
        "${LABEL_ARGS[@]}" \
        --json number,title,headRefName,author,statusCheckRollup \
        --limit 100)

    # Filter PRs with failed checks and format for display
    failed_prs=$(echo "$prs_json" | jq -r '
        .[] |
        select(.statusCheckRollup // [] | any(.conclusion == "FAILURE" or .conclusion == "TIMED_OUT" or .conclusion == "STARTUP_FAILURE" or .conclusion == "ACTION_REQUIRED")) |
        {
            number: .number,
            title: .title,
            branch: .headRefName,
            author: .author.login,
            failed_checks: [.statusCheckRollup[] | select(.conclusion == "FAILURE" or .conclusion == "TIMED_OUT" or .conclusion == "STARTUP_FAILURE" or .conclusion == "ACTION_REQUIRED")]
        } |
        "#\(.number) | \(.title) | @\(.author) | \(.branch) | \(.failed_checks | length) failed"
    ')

    if [ -z "$failed_prs" ]; then
        echo -e "${GREEN}No pull requests with failed checks found!${NC}"
        exit 0
    fi

    echo -e "${YELLOW}Found pull requests with failed checks:${NC}" >&2
    echo ""

    # Use fzf to select PRs
    selected_prs=$(echo "$failed_prs" | fzf \
        --multi \
        --ansi \
        --header="Select pull requests to restart failed workflows (TAB to select multiple, ENTER to confirm)" \
        --preview="pr_number=\$(echo {} | cut -d'|' -f1 | tr -d '# '); gh pr checks ${REPO_FLAG[*]} \"\$pr_number\" 2>/dev/null | grep -E '(fail|FAILURE|×)' || echo 'Loading...'" \
        --preview-window=right:60%:wrap \
        --bind='ctrl-/:toggle-preview' \
        --height=80%)

    if [ -z "$selected_prs" ]; then
        echo -e "${YELLOW}No pull requests selected.${NC}"
        exit 0
    fi
fi

echo ""
echo -e "${BLUE}Processing selected pull requests...${NC}"
echo ""

# Process each selected PR
while IFS= read -r pr_line; do
    pr_number=$(echo "$pr_line" | cut -d'|' -f1 | tr -d '# ' | xargs)
    pr_title=$(echo "$pr_line" | cut -d'|' -f2 | xargs)
    pr_branch=$(echo "$pr_line" | cut -d'|' -f4 | xargs)

    echo -e "${BLUE}PR #$pr_number: $pr_title${NC}"

    # Build jq ignore filter
    ignore_filter=""
    for pattern in "${IGNORE_PATTERNS[@]}"; do
        if [ -n "$ignore_filter" ]; then
            ignore_filter="$ignore_filter and "
        fi
        ignore_filter="${ignore_filter}(.name | contains(\"$pattern\") | not)"
    done

    # Get failed workflow runs for this PR using the branch
    failed_runs=$(gh run list "${REPO_FLAG[@]}" \
        --branch "$pr_branch" \
        --json databaseId,name,conclusion,status,event \
        --limit 50 \
        | jq -r "
        .[] |
        select(.event == \"pull_request\" and (.conclusion == \"failure\" or .conclusion == \"timed_out\" or .conclusion == \"startup_failure\" or .conclusion == \"action_required\") and ($ignore_filter)) |
        \"\(.databaseId)|\(.name)|\(.conclusion)\"")

    if [ -z "$failed_runs" ]; then
        echo -e "${YELLOW}  No failed workflow runs found (may have been restarted already)${NC}"
        continue
    fi

    # Restart all failed workflow runs
    echo -e "${YELLOW}  Restarting failed workflows:${NC}"

    echo "$failed_runs" | while IFS='|' read -r run_id workflow_name status; do
        echo -e "  ${GREEN}→${NC} Restarting: $workflow_name ($status)"

        rerun_output=$(gh run rerun "${REPO_FLAG[@]}" "$run_id" --failed 2>&1)

        if echo "$rerun_output" | grep -q "created over a month ago"; then
            echo -e "    ${YELLOW}⚠${NC} Cannot restart: workflow run is too old (>1 month)"
        elif echo "$rerun_output" | grep -qi "error"; then
            echo -e "    ${RED}✗${NC} Failed to restart: $rerun_output"
        else
            echo -e "    ${GREEN}✓${NC} Restarted successfully"
        fi
    done

    echo ""
done <<< "$selected_prs"

echo -e "${GREEN}Done!${NC}"
