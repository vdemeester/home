---
name: Org
description: Org-mode file manipulation using Emacs batch mode. USE WHEN you need to programmatically read, parse, or modify org-mode files (.org) for TODOs, notes, or other structured content.
---

# Org-Mode File Manipulation

## Purpose
Provide reliable, programmatic access to org-mode files using Emacs batch mode and the org-element API. This skill is used by other skills (TODOs, Notes) for org-mode operations.

### Context Detection

**This skill activates when:**
- Other skills need to manipulate org-mode files
- Parsing TODO items, denote notes, or org content
- Updating TODO states, scheduling, or properties
- Querying org-mode structure or metadata
- Working with files ending in `.org`

## Tool: org-manager

### Location
`tools/org-manager` - Bash CLI wrapper around Emacs batch mode

### Usage

```bash
# List TODOs
./tools/org-manager list ~/desktop/org/todos.org --state=NEXT

# Add TODO
./tools/org-manager add ~/desktop/org/todos.org "Task name" \
  --section=Work --priority=2 --scheduled=2025-12-10

# Update state
./tools/org-manager update-state ~/desktop/org/todos.org "Task name" DONE

# Count by state
./tools/org-manager count ~/desktop/org/todos.org

# Get scheduled items
./tools/org-manager scheduled ~/desktop/org/todos.org

# Search
./tools/org-manager search ~/desktop/org/todos.org "term"
```

### Output Format

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
      "scheduled": "2025-12-05",
      "deadline": null
    }
  ]
}
```

## Implementation

### Core Functions (batch-functions.el)

- `org-batch-list-todos` - Parse and filter TODOs
- `org-batch-scheduled-today` - Get scheduled items
- `org-batch-by-section` - Filter by section
- `org-batch-count-by-state` - Count statistics
- `org-batch-search` - Full-text search
- `org-batch-add-todo` - Add new TODO
- `org-batch-update-state` - Change states
- `org-batch-schedule-task` - Set SCHEDULED
- `org-batch-set-deadline` - Set DEADLINE
- `org-batch-set-priority` - Set priority
- `org-batch-archive-done` - Archive items

### Configuration

TODO keywords and priorities are configured for your setup:

```elisp
(setq org-todo-keywords
      '((sequence "STRT(s)" "NEXT(n)" "TODO(t)" "WAIT(w)" "|" "DONE(d!)" "CANX(c@/!)")))

(setq org-priority-highest 1
      org-priority-lowest 5
      org-priority-default 4)
```

## Performance

Tested on 354-item todos.org:
- Parse: <100ms
- Filter: <50ms
- Updates: <100ms per item

## References

- [[file:~/desktop/org/notes/20251205T092927--emacs-batch-mode-for-org-automation__emacs_orgmode_automation_elisp_reference.org][Research Note]]
- See `README.md` for full documentation
