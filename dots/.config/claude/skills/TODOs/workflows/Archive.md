# Archive TODO Workflow

## Purpose
Move completed (DONE) and cancelled (CANX) TODOs to archive sections to keep the main view clean.

## When to Use
- After marking tasks as DONE
- During weekly review
- When sections get cluttered
- Before starting new work phases

## Archive Structure

Each main section has its own archive:

| Section | Archive Location |
|---------|------------------|
| Work | `archive/work::` |
| Projects | `archive/projects::` |
| Systems | `archive/systems::` |
| Personal | `archive/personal::` |
| Routines | `archive/routines::` |
| Health | `archive/health::` |
| Appointments | `archive/appointments::` |

Archive is defined in the section's PROPERTIES:
```org
* Work
:PROPERTIES:
:ARCHIVE:  archive/work::
:CATEGORY: work
:END:
```

## What to Archive

### Always Archive
- ✅ TODOs marked as DONE
- ✅ TODOs marked as CANX
- ✅ Completed subtasks of projects
- ✅ Past recurring events (older than 2 weeks)

### Sometimes Archive
- ⚠️ Old WAIT tasks (if no longer relevant)
- ⚠️ Stale TODOs (created >3 months ago, never started)

### Never Archive
- ❌ Active TODOs (TODO, NEXT, STRT, WAIT)
- ❌ Recent recurring tasks
- ❌ Project headers with active subtasks

## Archive Workflow

### Manual Archive (Emacs)
If using Emacs with org-mode:
```
1. Position cursor on DONE/CANX heading
2. Press C-c C-x C-a (org-archive-subtree)
3. Item moves to archive file
```

### Manual Archive (Command Line)

#### Find Completed Items
```bash
# Find DONE items in Work section
sed -n '/^\* Work/,/^\* Projects/p' ~/desktop/org/todos.org | \
  grep -B5 "^** DONE"

# Find CANX items in Work section
sed -n '/^\* Work/,/^\* Projects/p' ~/desktop/org/todos.org | \
  grep -B5 "^** CANX"
```

#### Archive Process
1. **Read the item** (with properties and notes)
2. **Determine archive location** (based on section)
3. **Copy to archive file** with structure
4. **Remove from todos.org**

### Batch Archive

#### Weekly Archive Script
```bash
#!/bin/bash
# Archive all DONE and CANX from last 7+ days

# This is a manual process - identify items and archive them
echo "=== Items to Archive ==="
echo ""
echo "DONE items:"
grep -B2 "^** DONE" ~/desktop/org/todos.org | \
  grep "CLOSED:" | \
  grep -v "$(date +%Y-%m-%d)"
echo ""
echo "CANX items:"
grep -B2 "^** CANX" ~/desktop/org/todos.org | \
  grep "CLOSED:" | \
  grep -v "$(date +%Y-%m-%d)"
```

## Archive File Structure

Archive files follow the pattern:
```
~/desktop/org/archive/<section>.org
```

Example: `~/desktop/org/archive/work.org`

### Archive File Format
```org
#+title: Work Archive
#+category: work

* Archived Tasks

** DONE [#2] Review upstream pull request
CLOSED: [2025-12-04 Thu 15:30]
:PROPERTIES:
:CREATED:       [2025-11-15 Mon 10:00]
:CATEGORY: work
:ARCHIVE_TIME: 2025-12-05 Fri 09:00
:ARCHIVE_FILE: ~/desktop/org/todos.org
:ARCHIVE_OLPATH: Work
:END:
:LOGBOOK:
- State "DONE"       from "NEXT"       [2025-12-04 Thu 15:30]
:END:

Reviewed and approved upstream PR #123

** CANX Setup alternative CI system
CLOSED: [2025-12-03 Wed 10:00]
:PROPERTIES:
:CREATED:       [2025-10-15 Wed 15:00]
:CATEGORY: work
:ARCHIVE_TIME: 2025-12-05 Fri 09:00
:ARCHIVE_FILE: ~/desktop/org/todos.org
:ARCHIVE_OLPATH: Work
:END:
:LOGBOOK:
- State "CANX"       from "TODO"       [2025-12-03 Wed 10:00] \\
  Decided to stick with current CI
:END:
```

## Archive Properties

When archiving, preserve and add:

**Original Properties**:
- CREATED - when TODO was created
- CATEGORY - section category
- All LOGBOOK entries

**Archive Properties** (added):
- ARCHIVE_TIME - when archived
- ARCHIVE_FILE - source file
- ARCHIVE_OLPATH - original section path

## Examples

### Example 1: Archive Single DONE Item

**In todos.org** (Work section):
```org
** DONE [#2] Fix authentication bug
CLOSED: [2025-12-04 Thu 15:30]
:PROPERTIES:
:CREATED:       [2025-11-20 Thu 10:00]
:CATEGORY: work
:END:
:LOGBOOK:
- State "DONE"       from "STRT"       [2025-12-04 Thu 15:30]
:END:

Fixed null pointer in validation
```

**After archiving**:
1. Copy entire entry to `~/desktop/org/archive/work.org`
2. Add ARCHIVE_* properties
3. Remove from todos.org Work section

### Example 2: Archive Completed Project

**In todos.org** (Projects section):
```org
** DONE Keyboard firmware improvements [3/3]
CLOSED: [2025-12-04 Thu 15:30]

*** DONE Leader key implementation
CLOSED: [2025-11-25 Tue 10:00]

*** DONE Nav/media layer standardization
CLOSED: [2025-12-02 Mon 14:00]

*** DONE Symbol combos
CLOSED: [2025-12-04 Thu 15:30]
```

**After archiving**:
- Entire project tree goes to `archive/projects.org`
- All subtasks preserved
- Original structure maintained

### Example 3: Weekly Batch Archive

**Process**:
1. Identify all DONE/CANX from past week
2. Group by section
3. Archive each group to appropriate archive file
4. Verify removal from todos.org

```bash
# Find this week's completed items
week_ago=$(date -d "7 days ago" +"%Y-%m-%d")
grep -B5 "CLOSED: \[$week_ago" ~/desktop/org/todos.org | \
  grep "^** \(DONE\|CANX\)"
```

## Searching Archived Items

### Find Archived TODO
```bash
# Search in all archives
grep -r "authentication bug" ~/desktop/org/archive/

# Search in specific archive
grep "project name" ~/desktop/org/archive/work.org
```

### Find by Date
```bash
# Items archived in December 2025
grep "ARCHIVE_TIME: 2025-12" ~/desktop/org/archive/work.org
```

### Find by Completion Date
```bash
# Items completed in November
grep "CLOSED: \[2025-11" ~/desktop/org/archive/work.org
```

## Archive Maintenance

### Monthly Review
Once per month:
1. Review archive files
2. Check for patterns (what got done?)
3. Identify what got cancelled (why?)
4. Extract learnings

### Yearly Cleanup
Once per year:
1. Archive old archive files (older than 1 year)
2. Compress if needed
3. Keep structure for reference

## Tips

1. **Archive weekly**: Don't let DONE items pile up
2. **Keep recent**: Archive items >7 days old
3. **Preserve context**: Keep all properties and notes
4. **Search later**: Archives are searchable history
5. **Learn from archives**: Review what you accomplished
6. **Don't delete**: Archives are valuable record
7. **Batch process**: Faster than one-by-one
8. **Verify before removing**: Make sure archived successfully

## Archive vs Delete

### Archive (Recommended)
- ✅ Completed work
- ✅ Cancelled with reason
- ✅ Historical record valuable
- ✅ May need to reference later

### Delete
- ❌ Duplicate entries
- ❌ Test/placeholder TODOs
- ❌ Accidental captures
- ❌ Spam/invalid entries

**Rule**: When in doubt, archive (disk space is cheap)

## Integration with Review

### During Weekly Review
1. **Archive last week's DONE**
   ```bash
   # Find week's completions
   grep -B5 "CLOSED: \[$(date -d '7 days ago' +%Y-%m-%d)" \
     ~/desktop/org/todos.org
   ```

2. **Review what got done**
   - What did you accomplish?
   - Any patterns?
   - Celebrate wins!

3. **Clean up todos.org**
   - Remove archived items
   - Keeps main file manageable

### During Monthly Review
1. **Review month's archives**
   ```bash
   grep "ARCHIVE_TIME: $(date +%Y-%m)" \
     ~/desktop/org/archive/*.org
   ```

2. **Extract insights**
   - High completion areas?
   - What got cancelled?
   - Time estimates accurate?

## Validation Checklist

Before archiving:
- [ ] Item is DONE or CANX
- [ ] Has CLOSED timestamp
- [ ] Closed more than 7 days ago (unless urgent cleanup)
- [ ] All properties preserved
- [ ] LOGBOOK entries included
- [ ] Archive destination correct

After archiving:
- [ ] Item in correct archive file
- [ ] ARCHIVE_* properties added
- [ ] Removed from todos.org
- [ ] Can find via search

## Common Questions

**Q: How long to keep archives?**
A: Indefinitely. They're your work history.

**Q: Archive incomplete projects?**
A: Only if completely abandoned. Usually mark as CANX first.

**Q: Archive recurring tasks?**
A: Only old completed instances (>2 weeks old).

**Q: Can I restore from archive?**
A: Yes, copy back to todos.org and remove ARCHIVE_* properties.

**Q: Archive immediately after DONE?**
A: No, keep recent completions visible for a week.

**Q: Archive file gets huge?**
A: Normal. Can split by year if needed (archive/work-2024.org).

## Anti-Patterns

❌ **Don't archive active TODOs**: Only DONE/CANX
❌ **Don't lose context**: Keep all properties and notes
❌ **Don't archive too quickly**: Keep recent completions visible
❌ **Don't skip ARCHIVE_* properties**: Important metadata
❌ **Don't delete instead of archive**: History is valuable
✅ **Do archive regularly**: Weekly is good rhythm
✅ **Do preserve structure**: Maintain original hierarchy
✅ **Do search archives**: They're a knowledge base
✅ **Do review archives**: Learn from your history
