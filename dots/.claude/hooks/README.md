# Claude Code Hooks

This directory contains hooks for Claude Code that automate various tasks and capture session information.

Adapted from [Personal AI Infrastructure](https://github.com/danielmiessler/Personal_AI_Infrastructure).

## Installed Hooks

### initialize-session.ts

**Purpose**: Session initialization

**Triggers**: SessionStart event

**What it does**:
- Detects and skips subagent sessions
- Sets terminal tab title to "Claude Ready"
- Logs session start to history
- Implements debouncing to prevent duplicate triggers

**Setup**:
```json
{
  "hooks": {
    "SessionStart": ["bun ~/.claude/hooks/initialize-session.ts"]
  }
}
```

### capture-tool-output.ts

**Purpose**: Tool execution logging

**Triggers**: PostToolUse event

**What it does**:
- Captures outputs from interesting tools (Bash, Edit, Write, Read, Task, etc.)
- Logs to JSONL files organized by year-month
- Stores in `~/.claude/history/tool-outputs/YYYY-MM/YYYY-MM-DD_tool-outputs.jsonl`
- Silent failure - doesn't disrupt workflow

**Setup**:
```json
{
  "hooks": {
    "PostToolUse": ["bun ~/.claude/hooks/capture-tool-output.ts"]
  }
}
```

**Captured tools**:
- Bash
- Edit, Write, Read
- Task
- NotebookEdit
- Skill, SlashCommand

### validate-docs.ts

**Purpose**: Documentation link validation

**Triggers**: Manual or pre-commit

**What it does**:
- Scans all markdown files for internal links
- Checks if linked files exist
- Reports broken links with file:line numbers
- Exit code 0 if valid, 1 if broken links found

**Usage**:
```bash
# Run manually
bun ~/.claude/hooks/validate-docs.ts

# Add to pre-commit (optional)
```

## Skipped Hooks

The following hooks from PAI were **not imported** as they require more setup or are not immediately needed:

### capture-session-summary.ts
**Reason**: Complex - requires parsing conversation output files and generating markdown summaries. Can be added later if needed.

### capture-all-events.ts
**Reason**: Very comprehensive event logging with agent metadata extraction. More complex than capture-tool-output. Can add if detailed event tracking is needed.

### self-test.ts
**Reason**: Validates PAI-specific directory structure and configurations. Would need significant adaptation for our setup.

### validate-protected.ts
**Reason**: Requires `.pai-protected.json` manifest defining protected file patterns. Can add later with custom protection rules.

## Hook Architecture

### Directory Structure

```
~/.claude/hooks/
├── README.md                    # This file
├── lib/
│   └── claude-paths.ts         # Path utilities
├── initialize-session.ts       # Session startup
├── capture-tool-output.ts      # Tool logging
└── validate-docs.ts            # Link validation
```

### Helper Library: lib/claude-paths.ts

Provides:
- `CLAUDE_DIR`: Base directory (~/.claude)
- `HOOKS_DIR`, `SKILLS_DIR`, `AGENTS_DIR`, `HISTORY_DIR`: Subdirectories
- `getHistoryFilePath(subdir, filename)`: Generate history file paths
- `getTimestamp()`: Get formatted timestamp
- Path validation on import

## Configuration

Hooks are configured in Claude Code settings (`.claude/settings.json` or `.claude/settings.local.json`):

```json
{
  "hooks": {
    "SessionStart": ["bun ~/.claude/hooks/initialize-session.ts"],
    "PostToolUse": ["bun ~/.claude/hooks/capture-tool-output.ts"]
  }
}
```

Available hook events:
- `SessionStart`: Session begins
- `SessionEnd`: Session ends
- `PreToolUse`: Before tool execution
- `PostToolUse`: After tool execution
- `UserPromptSubmit`: After user submits prompt
- `Stop`: Session stopped
- `SubagentStop`: Subagent stopped
- `PreCompact`: Before context compaction
- `Notification`: System notification

## Requirements

- **Bun**: All hooks use `#!/usr/bin/env bun` shebang
- Install: `curl -fsSL https://bun.sh/install | bash`
- Already installed on kyushu system

## History Logging

Hooks that capture data store it in `~/.claude/history/`:

```
~/.claude/history/
├── sessions/
│   └── YYYY-MM/
│       └── YYYY-MM-DD_session-log.txt
└── tool-outputs/
    └── YYYY-MM/
        └── YYYY-MM-DD_tool-outputs.jsonl
```

This integrates with the history system documented in `.claude/skills/CORE/history-system.md`.

## Adding More Hooks

To add additional hooks:

1. Create the hook file in `~/.claude/hooks/`
2. Add shebang: `#!/usr/bin/env bun`
3. Make it executable: `chmod +x hook-name.ts`
4. Use `lib/claude-paths.ts` for paths
5. Add to settings.json under appropriate event
6. Test manually before enabling

## Troubleshooting

**Hook not running:**
- Check settings.json syntax
- Verify file is executable (`ls -la ~/.claude/hooks/`)
- Check hook output in stderr
- Verify Bun is installed: `bun --version`

**Permission errors:**
- Ensure directories exist: `mkdir -p ~/.claude/history/{sessions,tool-outputs}`
- Check write permissions

**Debugging:**
- Hooks write to stderr - check terminal output
- Add debug logging: `console.error('[hook-name] Debug message')`
- Run manually: `bun ~/.claude/hooks/hook-name.ts`

## Future Enhancements

Consider adding:
- Session summary generation (port capture-session-summary.ts)
- Protected file validation (port validate-protected.ts)
- Custom event logging for specific workflows
- Integration with external notification systems
- Automatic documentation generation
