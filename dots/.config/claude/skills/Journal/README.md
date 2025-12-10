# Journal Skill

Claude Code skill for managing Journelly-format journal entries.

## Overview

This skill enables Claude to create and manage journal entries in the Journelly org-mode format. Journelly is an iOS app that stores journal entries as org-mode headings in a single file with optional GPS/weather metadata.

## Components

### SKILL.md
The skill definition and documentation. Contains:
- Journelly format specification
- Usage examples
- Best practices
- Integration patterns

### tools/journelly-batch-functions.el
Emacs Lisp batch functions for programmatic journal manipulation:
- `journelly-batch-create-entry` - Create new entry
- `journelly-batch-append-to-today` - Append to today's entry
- `journelly-batch-list-entries` - List recent entries
- `journelly-batch-search` - Search content
- `journelly-batch-get-entry` - Get specific entry by date

### tools/journelly-manager
Bash CLI wrapper around the Emacs batch functions. Provides a user-friendly interface for all journal operations.

## Usage

### Via Claude

Simply ask Claude to create journal entries:

```
"Create a journal entry about today's work on the Journal skill"
"Add to today's journal that I finished the implementation"
"Search my journal for entries about homelab"
"Show me my last 5 journal entries"
```

### Direct CLI Usage

```bash
# Create entry
~/.config/claude/skills/Journal/tools/journelly-manager create \
  ~/desktop/org/Journelly.org "Home" "Today was productive!"

# Create with weather
~/.config/claude/skills/Journal/tools/journelly-manager create \
  ~/desktop/org/Journelly.org "Kyushu" "Work notes" \
  --latitude=48.8672 --longitude=2.1851 \
  --temperature="15,2°C" --condition="Cloudy" --symbol="cloud"

# Append to today
~/.config/claude/skills/Journal/tools/journelly-manager append \
  ~/desktop/org/Journelly.org "Additional thoughts for the day"

# List recent entries
~/.config/claude/skills/Journal/tools/journelly-manager list \
  ~/desktop/org/Journelly.org --limit=5

# Search
~/.config/claude/skills/Journal/tools/journelly-manager search \
  ~/desktop/org/Journelly.org "claude"

# Get specific entry
~/.config/claude/skills/Journal/tools/journelly-manager get \
  ~/desktop/org/Journelly.org "2025-12-08"
```

## Journelly Format

Journal entries follow this structure:

```org
* [YYYY-MM-DD Day HH:MM] @ Location
:PROPERTIES:
:LATITUDE: 48.86721377062119
:LONGITUDE: 2.1850910842231994
:WEATHER_TEMPERATURE: 5,8°C
:WEATHER_CONDITION: Cloudy
:WEATHER_SYMBOL: cloud
:END:
Entry content goes here...

- Section Title (use indented lists, NOT sub-headings)
  - Item 1
  - Item 2
```

- **Reverse chronological**: Newest entries at top
- **Optional properties**: GPS/weather metadata from iOS app
- **Content format**: Org-mode support (lists, links, code blocks)
- **NO sub-headings**: Use indented lists instead of `**` level 2 headings
- **Single file**: All entries in `~/desktop/org/Journelly.org`

## Integration

### With Journelly iOS App
- Primary mobile interface for journal
- Automatically adds GPS and weather data
- Syncs via iCloud
- Photos captured on iPhone

### With Claude Code
- Create entries via natural language
- Search and review past entries
- Append to existing entries
- Generate journal entries from work sessions

### With Syncthing
- Sync across devices
- Available on desktop and mobile
- Changes sync bidirectionally

## Testing

Test the journelly-manager tool:

```bash
# Show help
~/.config/claude/skills/Journal/tools/journelly-manager --help

# List entries (read-only)
~/.config/claude/skills/Journal/tools/journelly-manager list \
  ~/desktop/org/Journelly.org --limit=3

# Search entries (read-only)
~/.config/claude/skills/Journal/tools/journelly-manager search \
  ~/desktop/org/Journelly.org "test"
```

## Development

The skill uses Emacs batch mode for reliable org-mode parsing and manipulation:

1. **journelly-batch-functions.el**: Core logic in Emacs Lisp
2. **journelly-manager**: Bash wrapper for CLI interface
3. **SKILL.md**: Documentation for Claude integration

All operations return JSON for programmatic integration.

## Related Skills

- **Notes**: Denote-format note-taking (multi-file, topic-based)
- **TODOs**: Task management with org-mode
- **Org**: Core org-mode manipulation library

The Journal skill complements these by providing time-based, personal reflections while Notes provides topic-based knowledge management.

## Version

1.0.0

## Author

Vincent Demeester <vincent@demeester.fr>

## License

Part of personal Claude Code configuration.
