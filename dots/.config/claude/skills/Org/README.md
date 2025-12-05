# Org Skill - Org-Mode File Manipulation

Programmatic org-mode file manipulation using Emacs batch mode and the org-element API.

## Overview

This skill provides reliable access to org-mode files for TODO management, note parsing, and structured content manipulation. Used by other Claude Code skills (TODOs, Notes) and can be used standalone.

## Tool: org-manager

CLI tool for org-mode operations via Emacs batch mode.

### Usage

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

- `tools/batch-functions.el` - Core elisp operations (343 lines)
- `tools/org-manager` - Bash CLI wrapper (450 lines)

### Functions

**Read operations:**
- `org-batch-list-todos` - Parse and filter TODOs
- `org-batch-scheduled-today` - Get scheduled items
- `org-batch-by-section` - Filter by section
- `org-batch-count-by-state` - Count statistics
- `org-batch-search` - Full-text search
- `org-batch-get-sections` - List sections

**Write operations:**
- `org-batch-add-todo` - Add new TODO
- `org-batch-update-state` - Change states
- `org-batch-schedule-task` - Set SCHEDULED
- `org-batch-set-deadline` - Set DEADLINE
- `org-batch-set-priority` - Set priority
- `org-batch-archive-done` - Archive items

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
