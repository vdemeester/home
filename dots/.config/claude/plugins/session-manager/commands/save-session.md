---
description: Save a summary of the current Claude Code session to history
---

# Save Session

Create a comprehensive session summary following the history-system format and save it to `~/.config/claude/history/sessions/`.

## Important Guidelines

- **DO NOT use echo or command-line tools to communicate** - output all text directly in your response
- **Create a properly formatted session entry** following the history-system skill template
- **Save to the correct location** with proper timestamp and filename format
- **Be concise but comprehensive** - capture what matters

---

## Workflow

### Step 1: Analyze the Session

Review the conversation and identify:
1. **Main accomplishments** - What was actually done?
2. **Key decisions** - What choices were made and why?
3. **Next steps** - What follow-up work is needed?
4. **Related topics** - What areas of the codebase or system were touched?

### Step 2: Generate Session Entry

Create a session entry file with:

**Filename format**: `YYYY-MM-DD-HHMMSS_SESSION_description.md`

**Location**: `~/.config/claude/history/sessions/YYYY-MM/`

**Template**:
```markdown
# Session: <Brief Description>

**Date**: YYYY-MM-DD HH:MM
**Duration**: <estimated duration if known>
**Context**: <What prompted this work session>

## What Was Accomplished
- <Specific task 1>
- <Specific task 2>
- <Specific task 3>

## Decisions Made
- **<Decision>**: <Rationale>
- **<Decision>**: <Rationale>

## Technical Details
<Any important technical information worth preserving>

## Next Steps
- [ ] <Follow-up task 1>
- [ ] <Follow-up task 2>

## Related Notes
<If applicable, link to related denote notes>

## Tags
#session #<relevant-topic-tags>
```

### Step 3: Save the File

Use the Write tool to save the session entry to:
```
~/.config/claude/history/sessions/YYYY-MM/YYYY-MM-DD-HHMMSS_SESSION_description.md
```

Replace:
- `YYYY-MM` with current year-month
- `YYYY-MM-DD-HHMMSS` with current timestamp
- `description` with a short hyphenated description (e.g., `nixos-syncthing-config`)

### Step 4: Confirm to User

After saving, inform the user:
```
✅ Session saved to: ~/.config/claude/history/sessions/YYYY-MM/YYYY-MM-DD-HHMMSS_SESSION_description.md

Summary:
- X tasks completed
- Y decisions documented
- Z next steps identified
```

---

## Examples

### Example 1: Configuration Change Session
```markdown
# Session: Configure Syncthing for Claude History

**Date**: 2025-12-04 07:30
**Duration**: 30 minutes
**Context**: Need to sync Claude Code history across machines for backup

## What Was Accomplished
- Added claude-history folder to globals.nix syncthingFolders
- Generated syncthing folder ID (j5zdn-6kq4t)
- Configured claude-history sync for kyushu and aomi
- Path: /home/vincent/.config/claude/history

## Decisions Made
- **Syncthing ID format**: Used random 5-char strings (xxxxx-xxxxx) following existing pattern
- **Machines**: Started with kyushu and aomi only, can add more later

## Technical Details
- Syncthing folder ID: j5zdn-6kq4t
- Path: /home/vincent/.config/claude/history
- Synced machines: kyushu (SBLRZF4-...), aomi (CN5P3MV-...)

## Next Steps
- [ ] Rebuild NixOS on kyushu to activate sync
- [ ] Rebuild NixOS on aomi to activate sync
- [ ] Verify syncthing picks up the new folder
- [ ] Consider adding more machines later

## Tags
#session #nixos #syncthing #claude-code #homelab
```

### Example 2: Development Session
```markdown
# Session: Implement Session Saving Hooks

**Date**: 2025-12-04 08:00
**Duration**: 45 minutes
**Context**: Automate session saving in Claude Code

## What Was Accomplished
- Created claude-hooks-save-session Go command
- Created /save-session slash command plugin
- Updated plugin structure in dots/.claude/plugins/session-manager
- Documented workflow in command file

## Decisions Made
- **SessionEnd hook approach**: Prompt user instead of auto-save (user agency)
- **Plugin structure**: Created dedicated session-manager plugin
- **Format**: Follow existing history-system markdown template

## Technical Details
- Hook location: tools/claude-hooks/cmd/save-session/
- Plugin location: dots/.claude/plugins/session-manager/
- Follows existing claude-hooks Go patterns

## Next Steps
- [ ] Update default.nix to build save-session command
- [ ] Add SessionEnd hook to settings.json
- [ ] Test hook and slash command
- [ ] Update setup-hooks.sh

## Tags
#session #development #claude-code #golang #hooks
```

---

## Best Practices

1. **Be specific** - Don't just say "worked on code", say what exactly was modified
2. **Capture decisions** - Document WHY choices were made, not just what was done
3. **Include context** - Future you needs to know what prompted this work
4. **Link related work** - Reference denote notes, commits, or other sessions
5. **Add tags** - Make sessions searchable by topic
6. **Keep it real** - If nothing significant happened, it's okay to skip saving

## When to Use

- After completing significant work
- When making important architectural decisions
- When learning something worth preserving
- At the end of productive sessions
- When asked by SessionEnd hook

## When to Skip

- Very short sessions (<5 minutes)
- Purely exploratory work with no findings
- When no meaningful progress was made
- When the work will be documented elsewhere
