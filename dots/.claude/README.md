# Claude Code Configuration

This directory contains Claude Code infrastructure that is shared across machines and tracked in git.

## Directory Structure

```
dots/.claude/
├── agents/              # Specialized AI agents
├── hooks/               # Event-driven automation hooks
├── skills/              # Modular AI capabilities
├── settings.json        # Shared settings (tracked in git)
└── README.md           # This file

~/.claude/
├── agents/             -> /home/vincent/src/home/dots/.claude/agents/
├── hooks/              -> /home/vincent/src/home/dots/.claude/hooks/
├── skills/             -> /home/vincent/src/home/dots/.claude/skills/
├── settings.json       -> /home/vincent/src/home/dots/.claude/settings.json
├── settings.local.json # Machine/project-specific (NOT tracked)
└── history/            # Captured sessions and logs (NOT tracked)
```

## Settings Strategy

### settings.json (Shared)

**Location**: `dots/.claude/settings.json` (symlinked to `~/.claude/settings.json`)

**Purpose**: Shared configuration across all machines

**Contains**:
- Hook configurations
- Enabled plugins list
- General preferences (alwaysThinkingEnabled, etc.)
- Shared MCP server configs

**Why shared**:
- Consistent hooks across machines
- Same plugins enabled everywhere
- Synchronized preferences
- Uses absolute paths that are consistent (`/home/vincent/.claude/...`)

### settings.local.json (Machine-Specific)

**Location**: `~/.claude/settings.local.json` or project `.claude/settings.local.json`

**Purpose**: Machine or project-specific overrides

**Contains**:
- Permissions (project-specific)
- Machine-specific paths
- Local environment variables
- Project-specific plugin configs

**Not tracked in git**: Machine/project-specific configuration

### Settings Precedence

Claude Code merges settings in this order:
1. `settings.json` (base configuration)
2. `settings.local.json` (overrides)

Local settings always take precedence over shared settings.

## Setup on New Machine

To set up Claude Code infrastructure on a new machine:

```bash
# Clone the home repository
cd ~/src/home

# Run make to create all symlinks
cd dots
make all

# This will link:
# - ~/.claude/skills/
# - ~/.claude/agents/
# - ~/.claude/hooks/
# - ~/.claude/settings.json

# Optionally create machine-specific settings
cat > ~/.claude/settings.local.json <<EOF
{
  "permissions": {
    "allow": [
      "Bash(*)"
    ]
  }
}
EOF
```

## Components

### Agents (agents/)

Specialized AI agents with specific expertise:
- **engineer** - Software engineering and implementation
- **architect** - Architecture and PRD creation
- **designer** - UX/UI design and accessibility
- **claude-researcher** - Web research with WebSearch
- **researcher** - Comprehensive multi-tool research

See `agents/` for individual agent documentation.

### Skills (skills/)

Modular capabilities that extend Claude Code:
- **CORE** - Core principles and operating system
- **Art** - Visual content and Mermaid diagrams
- **Createskill** - Skill creation and validation
- **golang** - Go development
- **homelab** - Infrastructure management
- **nix** - NixOS configuration
- **notes** - Note-taking with denote

See each skill's `SKILL.md` for documentation.

### Hooks (hooks/)

Event-driven automation:
- **initialize-session.ts** - Session startup
- **capture-tool-output.ts** - Tool execution logging
- **validate-docs.ts** - Documentation link validation

See `hooks/README.md` for detailed documentation.

## Makefile Targets

```bash
# Link all Claude Code infrastructure
make claude-skills     # Link skills
make claude-agents     # Link agents
make claude-hooks      # Link hooks
make claude-settings   # Link settings.json

# Link everything
make all
```

## History System

History is **not** tracked in git and stays local to each machine:

```
~/.claude/history/
├── sessions/          # Session summaries
├── learnings/         # Problem-solving insights
├── research/          # Deep investigations
├── decisions/         # Architecture decisions
└── tool-outputs/      # Captured tool executions (via hooks)
```

See `skills/CORE/history-system.md` for details.

## Permissions

Permissions are intentionally kept in `settings.local.json` because:
- Different projects need different permissions
- Security policies may vary by machine
- Easier to audit per-project permissions

Example project-specific permissions:
```json
{
  "permissions": {
    "allow": [
      "Bash(make:*)",
      "Bash(nix:*)",
      "WebFetch(domain:docs.example.com)"
    ]
  }
}
```

## Benefits of This Approach

1. **Consistency**: Same agents, skills, and hooks everywhere
2. **Version Control**: Infrastructure tracked in git
3. **Flexibility**: Machine-specific overrides when needed
4. **Portability**: Easy to set up on new machines
5. **Maintainability**: Single source of truth for shared config

## Troubleshooting

**Symlinks not working?**
```bash
cd ~/src/home/dots
make all
```

**Settings not loading?**
```bash
# Check symlink
ls -la ~/.claude/settings.json

# Should point to: /home/vincent/src/home/dots/.claude/settings.json
```

**Hooks not running?**
```bash
# Check settings.json has hooks configured
cat ~/.claude/settings.json | grep -A 10 hooks

# Verify hooks are executable
ls -la ~/.claude/hooks/*.ts
```

## Related Documentation

- **Agents**: See individual agent `.md` files
- **Skills**: See each skill's `SKILL.md`
- **Hooks**: See `hooks/README.md`
- **History System**: See `skills/CORE/history-system.md`
