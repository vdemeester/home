# Org Skill - Org-Mode File Manipulation

Programmatic org-mode file manipulation using Emacs batch mode and the org-element API.

## Overview

This skill provides reliable access to org-mode files for TODO management, note parsing, and structured content manipulation. Used by other Claude Code skills (TODOs, Notes) and can be used standalone.

## Tool: org-manager

CLI tool for org-mode and denote operations via Emacs batch mode.

### TODO Operations

```bash
# List TODOs
./tools/org-manager list ~/desktop/org/todos.org --state=NEXT

# Count by state
./tools/org-manager count ~/desktop/org/todos.org

# Get scheduled items
./tools/org-manager scheduled ~/desktop/org/todos.org

# Add TODO
./tools/org-manager add ~/desktop/org/todos.org "Task name" \
  --section=Work --priority=2 --scheduled=2025-12-10

# Update state
./tools/org-manager update-state ~/desktop/org/todos.org "Task name" DONE
```

### Denote Operations

```bash
# Create denote-formatted note
./tools/org-manager denote-create "Note Title" "tag1,tag2,tag3" \
  --category=homelab --directory=~/desktop/org/notes

# Create with signature (automated notes)
./tools/org-manager denote-create "Session Log" "history,session" \
  --signature=pkai --category=history

# Read note metadata
./tools/org-manager denote-metadata ~/desktop/org/notes/20251205T*.org

# Update note frontmatter
./tools/org-manager denote-update ~/desktop/org/notes/20251205T*.org \
  --title="New Title" --tags="new,tags"

# Append content to note
echo "* New Section" > /tmp/content.org
./tools/org-manager denote-append ~/desktop/org/notes/20251205T*.org /tmp/content.org
```

### Output

All commands return JSON:

```json
{
  "success": true,
  "data": [
    {
      "heading": "Task name",
      "todo": "NEXT",
      "priority": 2,
      "tags": ["tag1"],
      "level": 2,
      "scheduled": "2025-12-05"
    }
  ]
}
```

## Implementation

### Files

- `tools/batch-functions.el` - Core elisp TODO operations (343 lines)
- `tools/denote-batch-functions.el` - Denote note creation and management (300 lines)
- `tools/org-manager` - Bash CLI wrapper (680 lines)

### Functions

**TODO Operations (batch-functions.el):**
- `org-batch-list-todos` - Parse and filter TODOs
- `org-batch-scheduled-today` - Get scheduled items
- `org-batch-by-section` - Filter by section
- `org-batch-count-by-state` - Count statistics
- `org-batch-search` - Full-text search
- `org-batch-get-sections` - List sections
- `org-batch-add-todo` - Add new TODO
- `org-batch-update-state` - Change states
- `org-batch-schedule-task` - Set SCHEDULED
- `org-batch-set-deadline` - Set DEADLINE
- `org-batch-set-priority` - Set priority
- `org-batch-archive-done` - Archive items

**Denote Operations (denote-batch-functions.el):**
- `denote-batch-create-note` - Create denote note with proper formatting
- `denote-batch-create-note-from-file` - Create note with content from file
- `denote-batch-append-content` - Append content to existing note
- `denote-batch-update-frontmatter` - Update note metadata
- `denote-batch-read-metadata` - Read note metadata as JSON

**Features:**
- Automatic timestamp generation (YYYYMMDDTHHMMSS)
- Signature support for automated notes (`==pkai`)
- Proper denote filename format: `TIMESTAMP==SIG--title__tags.org`
- Org-mode frontmatter generation (#+title, #+date, #+filetags, etc.)
- JSON output for all operations

## Performance

Tested on 354-item todos.org:
- Parse: <100ms
- Filter: <50ms
- Updates: <100ms per item

## Configuration

Configured for your TODO setup:

```elisp
(setq org-todo-keywords
      '((sequence "STRT" "NEXT" "TODO" "WAIT" "|" "DONE" "CANX")))

(setq org-priority-highest 1
      org-priority-lowest 5)
```

## References

- [[file:~/desktop/org/notes/20251205T092927--emacs-batch-mode-for-org-automation__emacs_orgmode_automation_elisp_reference.org][Research Note: Emacs Batch Mode for Org Automation]]
