#!/usr/bin/env bash
#
# setup-hooks.sh - Configure Claude Code hooks to use Nix-built binaries
#
# This script updates ~/.claude/settings.json to reference the correct
# hook binaries from the Nix store (via PATH).
#
# Usage:
#   ./setup-hooks.sh              # Update settings.json
#   ./setup-hooks.sh --dry-run    # Show what would be changed

set -euo pipefail

SETTINGS_FILE="${HOME}/.claude/settings.json"
SETTINGS_BACKUP="${HOME}/.claude/settings.json.backup"

# Determine if this is a dry run
DRY_RUN=false
if [[ "${1:-}" == "--dry-run" ]]; then
    DRY_RUN=true
fi

# Check if settings.json exists
if [[ ! -f "$SETTINGS_FILE" ]]; then
    echo "Error: $SETTINGS_FILE does not exist"
    echo "Please create it first or run Claude Code to initialize it"
    exit 1
fi

# Backup existing settings
if [[ "$DRY_RUN" == false ]]; then
    cp "$SETTINGS_FILE" "$SETTINGS_BACKUP"
    echo "Backed up existing settings to: $SETTINGS_BACKUP"
fi

# Read current settings
CURRENT_SETTINGS=$(cat "$SETTINGS_FILE")

# Build new hooks configuration
# Note: The binaries should be in PATH after installing via Nix
NEW_HOOKS=$(cat <<'EOF'
{
  "hooks": [
    {
      "type": "command",
      "command": "claude-hooks-initialize-session"
    }
  ]
}
EOF
)

POST_TOOL_HOOKS=$(cat <<'EOF'
{
  "hooks": [
    {
      "type": "command",
      "command": "claude-hooks-capture-tool-output"
    }
  ]
}
EOF
)

# Use jq to update the settings.json with proper JSON merging
UPDATED_SETTINGS=$(echo "$CURRENT_SETTINGS" | jq --argjson sessionStart "$NEW_HOOKS" \
    --argjson postTool "$POST_TOOL_HOOKS" \
    '.hooks.SessionStart = [$sessionStart] | .hooks.PostToolUse = [$postTool]')

if [[ "$DRY_RUN" == true ]]; then
    echo "=== DRY RUN - No changes made ==="
    echo ""
    echo "Would update $SETTINGS_FILE with:"
    echo "$UPDATED_SETTINGS" | jq '.'
    echo ""
    echo "Run without --dry-run to apply these changes"
else
    echo "$UPDATED_SETTINGS" | jq '.' > "$SETTINGS_FILE"
    echo "Successfully updated $SETTINGS_FILE"
    echo ""
    echo "Hook binaries configured:"
    echo "  - SessionStart: claude-hooks-initialize-session"
    echo "  - PostToolUse: claude-hooks-capture-tool-output"
    echo ""
    echo "Make sure claude-hooks is installed:"
    echo "  nix profile install .#claude-hooks"
    echo ""
    echo "Or add to your home-manager configuration:"
    echo "  home.packages = [ pkgs.claude-hooks ];"
fi
