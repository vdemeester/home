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

#### TODO Operations
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

#### Denote Operations
```bash
# Create denote-formatted note
./tools/org-manager denote-create "My Note Title" "tag1,tag2,tag3" \
  --category=homelab --directory=~/desktop/org/notes

# Create with signature (for automated notes)
./tools/org-manager denote-create "Session Log" "history,session" \
  --signature=pkai --category=history

# Read note metadata
./tools/org-manager denote-metadata ~/desktop/org/notes/20251205T*.org

# Update note frontmatter
./tools/org-manager denote-update ~/desktop/org/notes/20251205T*.org \
  --title="New Title" --tags="new,tags" --category="updated"

# Append content to note
echo "* New Section" > /tmp/content.org
./tools/org-manager denote-append ~/desktop/org/notes/20251205T*.org /tmp/content.org
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

**TODO Operations:**
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

### Denote Functions (denote-batch-functions.el)

**Note Creation and Management:**
- `denote-batch-create-note` - Create denote note with proper naming and frontmatter
- `denote-batch-create-note-from-file` - Create note with content from file
- `denote-batch-append-content` - Append content to existing note
- `denote-batch-update-frontmatter` - Update note metadata (title, tags, category)
- `denote-batch-read-metadata` - Read note metadata as JSON

**Features:**
- Automatic timestamp generation (YYYYMMDDTHHMMSS)
- Signature support for automated notes (e.g., `==pkai`)
- Proper denote filename format: `TIMESTAMP==SIG--title__tags.org`
- Org-mode frontmatter generation (#+title, #+date, #+filetags, etc.)
- JSON output for programmatic integration

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

## Examples

**Example 1: Reading and parsing org file**
```
User: "What TODOs are in my project.org file?"
→ Uses Emacs batch mode to parse org file
→ Extracts TODO items with org-element-map
→ Returns formatted list with priorities and tags
→ Shows deadlines and scheduled dates
→ Result: Complete overview of project TODOs
```

**Example 2: Updating org file programmatically**
```
User: "Mark all DONE items as archived"
→ Reads org file with Emacs batch mode
→ Finds all DONE entries
→ Moves them to archive section
→ Preserves timestamps and properties
→ Saves updated file
→ Result: Clean org file with archived history
```

**Example 3: Extracting information from org**
```
User: "Get all meeting notes from last month"
→ Parses org files for date range
→ Filters entries with :meeting: tag
→ Extracts content and metadata
→ Formats as summary report
→ Result: Month's meeting notes compiled
```
