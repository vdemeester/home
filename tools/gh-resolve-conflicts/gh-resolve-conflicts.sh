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
Usage: gh-resolve-conflicts [OPTIONS] [REPOSITORY[#PR_NUMBER]]

List pull requests with merge conflicts and resolve them interactively.

Options:
    -w, --worktree DIR     Create worktrees in DIR (default: /tmp/gh-resolve-conflicts-worktrees)
    -n, --no-worktree      Use existing repo (cd into it) instead of creating worktrees
    -N, --no-push          Do NOT automatically force-push after resolution (default: auto-push)
    -o, --org ORG          Filter PRs by organization
    -a, --author AUTHOR    Filter PRs by author (default: @me)
    -h, --help             Show this help message

Arguments:
    REPOSITORY    Optional repository in OWNER/REPO format.
                  If not provided, searches across all repos (when -o is used).
                  Can include #PR_NUMBER to directly resolve a specific PR.

Dependencies:
    - gh (GitHub CLI)
    - fzf (fuzzy finder, for interactive mode)
    - jq (JSON processor)
    - git
    - emacs (for ediff conflict resolution)

Examples:
    gh-resolve-conflicts                                    # List all your conflicting PRs (interactive)
    gh-resolve-conflicts -o tektoncd                        # List conflicting PRs in tektoncd org
    gh-resolve-conflicts owner/repo#123                     # Directly resolve PR #123
    gh-resolve-conflicts -N                                 # Don't auto-push after resolution
    gh-resolve-conflicts -n                                 # Use existing repo, no worktree

Workflow:
    1. Scans for PRs with merge conflicts
    2. Interactive selection with fzf (multi-select supported)
    3. For each PR:
       - Creates worktree or uses existing repo
       - Checks out PR branch
       - Attempts rebase against base branch
       - Launches emacs ediff for conflicts
       - Continues rebase after resolution
       - Optionally force-pushes

EOF
    exit 0
}

# Check dependencies
check_dependencies() {
    local missing=()

    for cmd in gh jq git; do
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

# Check if emacs is available
check_emacs() {
    if ! command -v emacs &> /dev/null; then
        echo -e "${YELLOW}Warning: emacs not found. Conflict resolution will use default git merge tool.${NC}" >&2
        return 1
    fi
    return 0
}

# Default settings
WORKTREE_DIR="/tmp/gh-resolve-conflicts-worktrees"
USE_WORKTREE=true
AUTO_PUSH=true
ORG_FILTER=""
AUTHOR_FILTER="@me"
REPO_ARG=""
PR_NUMBER=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -w|--worktree)
            if [ -n "${2:-}" ]; then
                WORKTREE_DIR="$2"
                shift 2
            else
                echo -e "${RED}Error: --worktree requires a directory argument${NC}" >&2
                exit 1
            fi
            ;;
        -n|--no-worktree)
            USE_WORKTREE=false
            shift
            ;;
        -N|--no-push)
            AUTO_PUSH=false
            shift
            ;;
        -o|--org)
            if [ -n "${2:-}" ]; then
                ORG_FILTER="$2"
                shift 2
            else
                echo -e "${RED}Error: --org requires an organization argument${NC}" >&2
                exit 1
            fi
            ;;
        -a|--author)
            if [ -n "${2:-}" ]; then
                AUTHOR_FILTER="$2"
                shift 2
            else
                echo -e "${RED}Error: --author requires an author argument${NC}" >&2
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

HAS_EMACS=false
if check_emacs; then
    HAS_EMACS=true
fi

# Fetch conflicting PRs
echo -e "${BLUE}Fetching pull requests with merge conflicts...${NC}" >&2

# Build search query
SEARCH_ARGS=(--author "$AUTHOR_FILTER" --state open)
if [ -n "$ORG_FILTER" ]; then
    SEARCH_ARGS+=(--owner "$ORG_FILTER")
fi

if [ -n "$PR_NUMBER" ]; then
    # Direct PR mode
    if [ -z "$REPO_ARG" ]; then
        echo -e "${RED}Error: Repository must be specified when using #PR_NUMBER${NC}" >&2
        exit 1
    fi

    echo -e "${BLUE}Fetching PR #$PR_NUMBER from $REPO_ARG...${NC}" >&2

    pr_info=$(gh pr view "$PR_NUMBER" -R "$REPO_ARG" \
        --json number,title,headRefName,baseRefName,author,mergeable,url \
        2>/dev/null)

    if [ -z "$pr_info" ]; then
        echo -e "${RED}Error: PR #$PR_NUMBER not found${NC}" >&2
        exit 1
    fi

    mergeable=$(echo "$pr_info" | jq -r '.mergeable')
    if [ "$mergeable" != "CONFLICTING" ]; then
        echo -e "${YELLOW}PR #$PR_NUMBER does not have merge conflicts (status: $mergeable)${NC}"
        exit 0
    fi

    pr_title=$(echo "$pr_info" | jq -r '.title')
    pr_branch=$(echo "$pr_info" | jq -r '.headRefName')
    base_branch=$(echo "$pr_info" | jq -r '.baseRefName')
    pr_author=$(echo "$pr_info" | jq -r '.author.login')
    pr_url=$(echo "$pr_info" | jq -r '.url')

    selected_prs="$REPO_ARG|#$PR_NUMBER|$pr_title|@$pr_author|$pr_branch|$base_branch|$pr_url"
else
    # Interactive mode: Search for conflicting PRs
    prs_json=$(gh search prs "${SEARCH_ARGS[@]}" \
        --json number,title,repository,url \
        --limit 100)

    if [ -z "$prs_json" ] || [ "$prs_json" = "[]" ]; then
        echo -e "${YELLOW}No open pull requests found.${NC}"
        exit 0
    fi

    # Check each PR for conflicts
    conflicting_prs=""
    total=$(echo "$prs_json" | jq 'length')
    current=0

    echo -e "${BLUE}Checking $total PRs for merge conflicts...${NC}" >&2

    while IFS= read -r pr; do
        ((current++)) || true
        repo=$(echo "$pr" | jq -r '.repository.nameWithOwner')
        number=$(echo "$pr" | jq -r '.number')
        title=$(echo "$pr" | jq -r '.title')
        url=$(echo "$pr" | jq -r '.url')

        echo -ne "${YELLOW}\rChecking PR $current/$total...${NC}" >&2

        # Fetch detailed PR info including mergeable status
        pr_details=$(gh pr view "$number" -R "$repo" \
            --json mergeable,headRefName,baseRefName,author 2>/dev/null || echo "{}")

        mergeable=$(echo "$pr_details" | jq -r '.mergeable // "UNKNOWN"')

        if [ "$mergeable" = "CONFLICTING" ]; then
            branch=$(echo "$pr_details" | jq -r '.headRefName')
            base_branch=$(echo "$pr_details" | jq -r '.baseRefName')
            author=$(echo "$pr_details" | jq -r '.author.login')
            conflicting_prs+="$repo|#$number|$title|@$author|$branch|$base_branch|$url"$'\n'
        fi
    done < <(echo "$prs_json" | jq -c '.[]')

    echo -e "\r${GREEN}Done checking PRs.${NC}                    " >&2

    if [ -z "$conflicting_prs" ]; then
        echo -e "${GREEN}No pull requests with merge conflicts found!${NC}"
        exit 0
    fi

    echo -e "${YELLOW}Found pull requests with merge conflicts:${NC}" >&2
    echo ""

    # Use fzf to select PRs
    selected_prs=$(echo "$conflicting_prs" | fzf \
        --multi \
        --ansi \
        --delimiter='|' \
        --with-nth=1,2,3,4 \
        --header="Select PRs to resolve conflicts (TAB for multi-select, ENTER to confirm)" \
        --preview="echo {} | cut -d'|' -f7 | xargs -I % echo 'URL: %'; echo ''; repo=\$(echo {} | cut -d'|' -f1); pr=\$(echo {} | cut -d'|' -f2 | tr -d '#'); gh pr diff \"\$pr\" -R \"\$repo\" 2>/dev/null | head -50 || echo 'Loading...'" \
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

# Function to resolve conflicts with emacs ediff
resolve_with_ediff() {
    local file="$1"

    if [ "$HAS_EMACS" = false ]; then
        echo -e "${YELLOW}Emacs not available, using git mergetool...${NC}"
        git mergetool "$file"
        return $?
    fi

    echo -e "${GREEN}Launching emacs ediff for: $file${NC}"

    # Create temporary elisp script for ediff
    local ediff_script
    ediff_script=$(mktemp)
    cat > "$ediff_script" <<'ELISP'
(defun resolve-conflict-and-quit ()
  "Resolve git conflict with ediff and quit when done."
  (let* ((file (car command-line-args-left))
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      ;; Check if file has conflict markers
      (goto-char (point-min))
      (if (search-forward "<<<<<<< " nil t)
          (progn
            ;; Use ediff-merge for 3-way merge
            (let* ((base-file (concat file ".base"))
                   (local-file (concat file ".LOCAL"))
                   (remote-file (concat file ".REMOTE")))
              (if (and (file-exists-p local-file)
                       (file-exists-p remote-file))
                  ;; If git created the temp files, use them
                  (ediff-merge-files-with-ancestor local-file remote-file base-file nil file)
                ;; Otherwise, try to extract from conflict markers
                (vc-resolve-conflicts))))
        ;; No conflict markers found
        (message "No conflict markers found in %s" file)))
    (setq command-line-args-left nil)))

(add-hook 'ediff-quit-hook
          (lambda ()
            (save-buffers-kill-terminal t)))

(resolve-conflict-and-quit)
ELISP

    # Launch emacs with ediff (using user's config)
    emacs --load "$ediff_script" "$file" 2>/dev/null

    rm -f "$ediff_script"

    # Check if conflict markers still exist
    if grep -q "<<<<<<< " "$file"; then
        echo -e "${RED}Conflict markers still present in $file${NC}"
        return 1
    fi

    # Mark as resolved
    git add "$file"
    return 0
}

# Function to resolve a single PR
resolve_pr() {
    local repo="$1"
    local pr_number="$2"
    local pr_title="$3"
    local pr_branch="$4"
    local base_branch="$5"
    local pr_url="$6"

    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Repository: $repo${NC}"
    echo -e "${BLUE}PR #$pr_number: $pr_title${NC}"
    echo -e "${BLUE}Branch: $pr_branch -> $base_branch${NC}"
    echo -e "${BLUE}URL: $pr_url${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""

    # Get PR details to find the head repository (fork)
    echo -e "${YELLOW}Finding fork repository...${NC}"
    pr_details=$(gh pr view "$pr_number" -R "$repo" \
        --json headRepository,headRepositoryOwner,isCrossRepository 2>/dev/null)

    if [ -z "$pr_details" ]; then
        echo -e "${RED}Failed to get PR details${NC}"
        return 1
    fi

    local is_cross_repo
    is_cross_repo=$(echo "$pr_details" | jq -r '.isCrossRepository')
    local fork_repo

    if [ "$is_cross_repo" = "true" ]; then
        # PR is from a fork - construct fork repo name
        local fork_owner
        fork_owner=$(echo "$pr_details" | jq -r '.headRepositoryOwner.login')
        local fork_name
        fork_name=$(echo "$pr_details" | jq -r '.headRepository.name')

        # Try to get full nameWithOwner, fall back to constructing it
        fork_repo=$(echo "$pr_details" | jq -r '.headRepository.nameWithOwner')
        if [ -z "$fork_repo" ] || [ "$fork_repo" = "null" ] || [ "$fork_repo" = "" ]; then
            fork_repo="$fork_owner/$fork_name"
        fi

        echo -e "${BLUE}PR is from fork: $fork_repo${NC}"
    else
        # PR is from same repo (branch)
        fork_repo="$repo"
        echo -e "${BLUE}PR is from branch in same repo${NC}"
    fi

    local work_dir

    if [ "$USE_WORKTREE" = true ]; then
        # Use worktree
        local repo_name
        repo_name=$(echo "$repo" | tr '/' '-')
        work_dir="$WORKTREE_DIR/$repo_name/pr-$pr_number"

        echo -e "${YELLOW}Creating worktree at: $work_dir${NC}"

        # Create parent directory
        mkdir -p "$WORKTREE_DIR/$repo_name"

        # Clone fork if not exists
        local repo_dir="$WORKTREE_DIR/$repo_name/main"
        if [ ! -d "$repo_dir" ]; then
            echo -e "${YELLOW}Cloning fork: $fork_repo...${NC}"
            gh repo clone "$fork_repo" "$repo_dir" -- --bare

            # Add upstream remote if this is a fork
            if [ "$is_cross_repo" = "true" ]; then
                echo -e "${YELLOW}Adding upstream remote: $repo...${NC}"
                git -C "$repo_dir" remote add upstream "https://github.com/$repo.git" 2>/dev/null || true
            fi
        else
            echo -e "${YELLOW}Fetching latest changes from fork...${NC}"
            git -C "$repo_dir" fetch origin

            # Ensure upstream remote exists if this is a fork
            if [ "$is_cross_repo" = "true" ]; then
                if ! git -C "$repo_dir" remote | grep -q "^upstream$"; then
                    echo -e "${YELLOW}Adding upstream remote: $repo...${NC}"
                    git -C "$repo_dir" remote add upstream "https://github.com/$repo.git"
                fi
            fi
        fi

        # Fetch from upstream if this is a fork
        if [ "$is_cross_repo" = "true" ]; then
            echo -e "${YELLOW}Fetching from upstream: $repo...${NC}"
            git -C "$repo_dir" fetch upstream
        fi

        # Remove existing worktree if present
        if [ -d "$work_dir" ]; then
            echo -e "${YELLOW}Removing existing worktree...${NC}"
            git -C "$repo_dir" worktree remove "$work_dir" --force 2>/dev/null || rm -rf "$work_dir"
        fi

        # Fetch PR branch from fork
        echo -e "${YELLOW}Fetching PR branch: $pr_branch...${NC}"
        git -C "$repo_dir" fetch origin "$pr_branch:pr-$pr_number" || {
            echo -e "${RED}Failed to fetch PR branch from fork${NC}"
            return 1
        }

        # Create new worktree
        echo -e "${YELLOW}Creating worktree for branch $pr_branch...${NC}"
        git -C "$repo_dir" worktree add "$work_dir" "pr-$pr_number" || {
            echo -e "${RED}Failed to create worktree${NC}"
            return 1
        }

        cd "$work_dir"
    else
        # Use existing repo
        echo -e "${YELLOW}Using existing repository (no worktree)${NC}"

        # Determine which repo we should be in
        current_repo=$(gh repo view --json nameWithOwner -q .nameWithOwner 2>/dev/null || echo "")

        # We should be in the fork, not upstream
        if [ "$current_repo" != "$fork_repo" ]; then
            echo -e "${RED}Error: Current directory is $current_repo, expected $fork_repo${NC}"
            echo -e "${YELLOW}Please cd to your fork or use --worktree mode${NC}"
            return 1
        fi

        work_dir=$(pwd)

        # Ensure upstream remote exists if this is a fork
        if [ "$is_cross_repo" = "true" ]; then
            if ! git remote | grep -q "^upstream$"; then
                echo -e "${YELLOW}Adding upstream remote: $repo...${NC}"
                git remote add upstream "https://github.com/$repo.git"
            fi
            echo -e "${YELLOW}Fetching from upstream...${NC}"
            git fetch upstream
        fi

        # Fetch PR
        echo -e "${YELLOW}Fetching PR #$pr_number...${NC}"
        gh pr checkout "$pr_number" -R "$repo" || {
            echo -e "${RED}Failed to checkout PR${NC}"
            return 1
        }
    fi

    # Determine the correct remote for base branch
    local base_remote
    if [ "$is_cross_repo" = "true" ]; then
        base_remote="upstream"
    else
        base_remote="origin"
    fi

    # Fetch base branch
    echo -e "${YELLOW}Fetching base branch from $base_remote: $base_branch${NC}"
    git fetch "$base_remote" "$base_branch" || {
        echo -e "${RED}Failed to fetch base branch${NC}"
        return 1
    }

    # Start rebase
    echo -e "${YELLOW}Starting rebase onto $base_remote/$base_branch...${NC}"
    echo ""

    if git rebase "$base_remote/$base_branch"; then
        echo -e "${GREEN}✓ Rebase completed successfully with no conflicts!${NC}"
    else
        echo -e "${YELLOW}Conflicts detected. Starting conflict resolution...${NC}"
        echo ""

        # Get list of conflicted files
        conflicted_files=$(git diff --name-only --diff-filter=U)

        if [ -z "$conflicted_files" ]; then
            echo -e "${RED}Error: Rebase failed but no conflicted files found${NC}"
            git rebase --abort
            return 1
        fi

        echo -e "${BLUE}Conflicted files:${NC}"
        echo "$conflicted_files" | while read -r file; do
            echo -e "  ${RED}✗${NC} $file"
        done
        echo ""

        # Resolve each conflict
        while read -r file; do
            echo -e "${BLUE}Resolving: $file${NC}"

            if ! resolve_with_ediff "$file"; then
                echo -e "${RED}Failed to resolve conflict in $file${NC}"
                echo -e "${YELLOW}Options:${NC}"
                echo -e "  ${YELLOW}1)${NC} Skip this file and continue"
                echo -e "  ${YELLOW}2)${NC} Abort rebase"
                echo -e "  ${YELLOW}3)${NC} Open file manually"
                read -rp "Choice [1-3]: " choice

                case $choice in
                    1)
                        echo -e "${YELLOW}Skipping $file${NC}"
                        continue
                        ;;
                    2)
                        echo -e "${YELLOW}Aborting rebase${NC}"
                        git rebase --abort
                        return 1
                        ;;
                    3)
                        ${EDITOR:-vim} "$file"
                        git add "$file"
                        ;;
                    *)
                        echo -e "${RED}Invalid choice, aborting${NC}"
                        git rebase --abort
                        return 1
                        ;;
                esac
            fi
        done <<< "$conflicted_files"

        # Continue rebase
        echo -e "${YELLOW}Continuing rebase...${NC}"
        if git rebase --continue; then
            echo -e "${GREEN}✓ Rebase completed successfully!${NC}"
        else
            echo -e "${RED}Failed to continue rebase${NC}"
            echo -e "${YELLOW}You may need to resolve remaining conflicts manually${NC}"
            echo -e "${YELLOW}Working directory: $work_dir${NC}"
            return 1
        fi
    fi

    echo ""
    echo -e "${GREEN}✓ Conflicts resolved successfully!${NC}"
    echo ""

    # Push changes
    if [ "$AUTO_PUSH" = true ]; then
        echo -e "${YELLOW}Force-pushing changes...${NC}"
        if git push --force-with-lease; then
            echo -e "${GREEN}✓ Changes pushed successfully!${NC}"
        else
            echo -e "${RED}Failed to push changes${NC}"
            echo -e "${YELLOW}You may need to push manually from: $work_dir${NC}"
            return 1
        fi
    else
        echo -e "${YELLOW}Changes not pushed. To push manually:${NC}"
        echo -e "  cd $work_dir"
        echo -e "  git push --force-with-lease"
    fi

    echo ""

    # Cleanup worktree
    if [ "$USE_WORKTREE" = true ]; then
        echo -e "${YELLOW}Note: Worktree kept at: $work_dir${NC}"
        echo -e "${YELLOW}To remove: git worktree remove $work_dir${NC}"
    fi

    return 0
}

# Process each selected PR
while IFS='|' read -r repo pr_number pr_title pr_author pr_branch base_branch pr_url; do
    pr_number=$(echo "$pr_number" | tr -d '#' | xargs)

    if ! resolve_pr "$repo" "$pr_number" "$pr_title" "$pr_branch" "$base_branch" "$pr_url"; then
        echo -e "${RED}Failed to resolve PR #$pr_number${NC}"
        echo ""
        continue
    fi

    echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
done <<< "$selected_prs"

echo -e "${GREEN}Done!${NC}"
