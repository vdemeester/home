#!/usr/bin/env bash
set -euo pipefail

# Automated NixOS flake.lock updater
# This script updates flake.lock, builds verification systems, and pushes to remote

# Configuration from environment or defaults
REPO_PATH="${REPO_PATH:-/home/vincent/src/home}"
FLAKE_PATH="${FLAKE_PATH:-$REPO_PATH}"
GIT_REMOTE="${GIT_REMOTE:-origin}"
BRANCH_PREFIX="${BRANCH_PREFIX:-flake-update-}"
NTFY_TOPIC="${NTFY_TOPIC:-nix-updates}"
NTFY_SERVER="${NTFY_SERVER:-https://ntfy.sh}"
BUILD_SYSTEMS="${BUILD_SYSTEMS:-}"
DRY_RUN="${DRY_RUN:-false}"

LOG_FILE="/var/log/nix-flake-updater/$(date +%Y%m%d-%H%M%S).log"
mkdir -p "$(dirname "$LOG_FILE")"

log() {
  echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "$LOG_FILE"
}

notify() {
  local priority="$1"
  local title="$2"
  local message="$3"
  local tags="$4"

  curl -s \
    -H "Title: $title" \
    -H "Priority: $priority" \
    -H "Tags: $tags" \
    -d "$message" \
    "$NTFY_SERVER/$NTFY_TOPIC" || true
}

cleanup() {
  local exit_code=$?
  if [ $exit_code -ne 0 ]; then
    log "ERROR: Update process failed with exit code $exit_code"
    notify "high" "❌ Flake Update Failed" \
      "Build failed. See logs: $LOG_FILE" \
      "warning,flake"
  fi
}

trap cleanup EXIT

log "Starting flake update process"
cd "$REPO_PATH"

# Ensure we're on main branch and up to date
log "Pulling latest changes from $GIT_REMOTE/main"
git fetch "$GIT_REMOTE"
git checkout main
git pull "$GIT_REMOTE" main

# Create update branch
BRANCH_NAME="$BRANCH_PREFIX$(date +%Y%m%d)"
log "Creating update branch: $BRANCH_NAME"

if git show-ref --verify --quiet "refs/heads/$BRANCH_NAME"; then
  log "Branch $BRANCH_NAME already exists, using unique name"
  BRANCH_NAME="$BRANCH_PREFIX$(date +%Y%m%d-%H%M%S)"
fi

git checkout -b "$BRANCH_NAME"

# Update flake.lock
log "Updating flake.lock"
cd "$FLAKE_PATH"
nix flake update 2>&1 | tee -a "$LOG_FILE"

# Check if there are changes
if ! git diff --quiet flake.lock; then
  log "Changes detected in flake.lock"

  # Show what changed
  log "Flake input changes:"
  git diff flake.lock | grep -E '^\+.*"(narHash|rev)"' | head -20 | tee -a "$LOG_FILE"

  # Build test systems
  BUILD_SUCCESS=true
  for system in $BUILD_SYSTEMS; do
    log "Building system: $system"
    if nix build "$FLAKE_PATH#nixosConfigurations.$system.config.system.build.toplevel" \
       --no-link \
       --print-build-logs 2>&1 | tee -a "$LOG_FILE"; then
      log "✓ $system built successfully"
    else
      log "✗ $system build failed"
      BUILD_SUCCESS=false
      break
    fi
  done

  if [ "$BUILD_SUCCESS" = true ]; then
    # Commit changes
    cd "$REPO_PATH"
    git add flake.lock

    # Generate commit message with changed inputs
    COMMIT_MSG="chore(flake): update flake.lock

$(nix flake metadata "$FLAKE_PATH" --json 2>/dev/null | \
  jq -r '.locks.nodes | to_entries[] | select(.key != "root") | "- \(.key): \(.value.locked.rev // .value.locked.narHash // "updated")"' 2>/dev/null || echo "Updated flake inputs")

🤖 Automated update
Built systems: $BUILD_SYSTEMS
"

    git commit -m "$COMMIT_MSG"

    if [ "$DRY_RUN" = "false" ]; then
      # Push to remote
      log "Pushing to $GIT_REMOTE/$BRANCH_NAME"
      git push "$GIT_REMOTE" "$BRANCH_NAME"

      # Notify success
      notify "default" "✅ Flake Updated Successfully" \
        "Branch $BRANCH_NAME created and pushed. All builds passed: $BUILD_SYSTEMS" \
        "white_check_mark,flake"

      log "SUCCESS: Flake updated and pushed to $BRANCH_NAME"
    else
      log "DRY RUN: Would push to $GIT_REMOTE/$BRANCH_NAME"
      notify "low" "🧪 Flake Update (Dry Run)" \
        "Branch $BRANCH_NAME created locally. All builds passed: $BUILD_SYSTEMS" \
        "test_tube,flake"
    fi

    # Return to main
    git checkout main

  else
    log "Build failed, not committing changes"
    notify "high" "❌ Flake Update Build Failed" \
      "Builds failed for updated flake.lock. Check logs: $LOG_FILE" \
      "x,flake,warning"

    # Clean up failed branch
    git checkout main
    git branch -D "$BRANCH_NAME"
    exit 1
  fi

else
  log "No changes in flake.lock, nothing to do"
  notify "low" "ℹ️ No Flake Updates" \
    "flake.lock is already up to date" \
    "information_source,flake"

  # Clean up unused branch
  git checkout main
  git branch -D "$BRANCH_NAME"
fi

log "Flake update process complete"
