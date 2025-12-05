#!/usr/bin/env bash
#
# Claude Code Statusline - Enhanced with NixOS/Homelab Context
#
# This statusline provides:
# - Line 1: Identity, model, project context, directory
# - Line 2: Active capabilities (skills, hooks)
#

# Read JSON input from stdin
input=$(cat)

# Extract data from JSON
current_dir=$(echo "$input" | jq -r '.workspace.current_dir')
model_name=$(echo "$input" | jq -r '.model.display_name')
dir_name=$(basename "$current_dir")

# Colors (simple ANSI for terminal compatibility)
PURPLE='\033[35m'
BLUE='\033[34m'
CYAN='\033[36m'
GREEN='\033[32m'
YELLOW='\033[33m'
RESET='\033[0m'

# Detect project context (check more specific paths first)
context="general"
if [[ "$current_dir" == *"/keyboards"* ]]; then
	context="${CYAN}keyboards${RESET}"
elif [[ "$current_dir" == *"/src/home"* ]]; then
	context="${YELLOW}homelab${RESET}"
fi

# Count active capabilities
claude_dir="${HOME}/.config/claude"

# Count skills (follow symlinks)
skills_count=0
if [ -d "$claude_dir/skills" ]; then
	skills_count=$(find -L "$claude_dir/skills" -name "SKILL.md" 2>/dev/null | wc -l | tr -d ' ')
fi

# Count agents
agents_count=0
if [ -d "$claude_dir/agents" ]; then
	agents_count=$(find -L "$claude_dir/agents" -maxdepth 1 -name "*.md" 2>/dev/null | wc -l | tr -d ' ')
fi

# LINE 1: Identity, model, context, directory
# shellcheck disable=SC2059
printf "👋 ${PURPLE}Vincent${RESET} • ${BLUE}${model_name}${RESET} • ${context} • ${CYAN}${dir_name}${RESET}\n"

# LINE 2: Capabilities
# shellcheck disable=SC2059
printf "${GREEN}Skills:${RESET} ${skills_count} ${GREEN}• Agents:${RESET} ${agents_count} ${GREEN}• Hooks:${RESET} active\n"
