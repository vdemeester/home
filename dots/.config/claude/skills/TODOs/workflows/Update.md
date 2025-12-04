# Update TODO Workflow

## Purpose
Modify existing TODO items: change state, update priority, reschedule, add notes.

## When to Use
- Mark task as done
- Change priority
- Reschedule task
- Add progress notes
- Update status
- Move to waiting

## Common Updates

### 1. Change State

**Mark as DONE**
```org
** DONE Review upstream pull request
CLOSED: [2025-12-04 Thu 15:30]
:LOGBOOK:
- State "DONE"       from "NEXT"       [2025-12-04 Thu 15:30]
:END:
```

**Mark as NEXT (from TODO)**
```org
** NEXT Setup MQTT broker on rhea
:LOGBOOK:
- State "NEXT"       from "TODO"       [2025-12-04 Thu 15:30]
:END:
```

**Mark as STRT (Started)**
```org
** STRT Keyboard firmware improvements
:LOGBOOK:
- State "STRT"       from "NEXT"       [2025-12-04 Thu 15:30]
:END:
```

**Mark as WAIT**
```org
** WAIT Fix authentication bug
:LOGBOOK:
- State "WAIT"       from "NEXT"       [2025-12-04 Thu 15:30] \\
  Waiting for security team review
:END:
```

**Mark as CANX (Cancelled)**
```org
** CANX Migrate to new framework
CLOSED: [2025-12-04 Thu 15:30]
:LOGBOOK:
- State "CANX"       from "TODO"       [2025-12-04 Thu 15:30] \\
  Decision to stay with current framework
:END:
```

### 2. Change Priority

**Increase priority** (lower number):
```org
** TODO [#1] Fix critical authentication bug
```

**Decrease priority** (higher number):
```org
** TODO [#4] Refactor old code
```

**Remove priority**:
```org
** TODO Update documentation
```

### 3. Reschedule

**Move to tomorrow**:
```org
** TODO Review pull request
SCHEDULED: <2025-12-05 Fri>
:LOGBOOK:
- Rescheduled from "[2025-12-04 Thu]" on [2025-12-04 Thu 15:30]
:END:
```

**Move to next week**:
```org
** TODO Prepare presentation
SCHEDULED: <2025-12-11 Thu>
:LOGBOOK:
- Rescheduled from "[2025-12-04 Thu]" on [2025-12-04 Thu 15:30]
:END:
```

**Remove scheduling**:
```org
** TODO Review documentation
:LOGBOOK:
- Rescheduled from "[2025-12-04 Thu]" on [2025-12-04 Thu 15:30]
:END:
```

### 4. Update Deadline

**Set new deadline**:
```org
** TODO Submit report
DEADLINE: <2025-12-10 Wed>
:LOGBOOK:
- New deadline from "[2025-12-05 Fri]" on [2025-12-04 Thu 15:30]
:END:
```

**Extend deadline**:
```org
** TODO Complete project
DEADLINE: <2025-12-15 Mon>
:LOGBOOK:
- New deadline from "[2025-12-10 Wed]" on [2025-12-04 Thu 15:30]
:END:
```

### 5. Add Progress Notes

**Add note to TODO**:
```org
** STRT Refactor authentication module
:LOGBOOK:
- Note taken on [2025-12-04 Thu 15:30] \\
  Completed initial analysis, starting implementation
- Note taken on [2025-12-03 Wed 10:00] \\
  Reviewed current implementation
:END:
```

**Add with context**:
```org
** TODO Deploy new configuration
:LOGBOOK:
- Note taken on [2025-12-04 Thu 15:30] \\
  Blocked: waiting for staging environment
:END:
```

## Update Patterns

### Find and Update

1. **Locate the TODO**
   ```bash
   # Search by description
   grep -n "Review upstream" ~/desktop/org/todos.org

   # Or by line number if known
   # Result: 142:** TODO Review upstream pull request
   ```

2. **Read the Context**
   ```bash
   # Show TODO with 5 lines of context
   sed -n '142,152p' ~/desktop/org/todos.org
   ```

3. **Update Using Edit Tool**
   - Find the exact TODO heading
   - Make the modification
   - Add LOGBOOK entry if state change
   - Update CLOSED timestamp if completing

### State Transition Rules

**Valid Transitions**:
```
TODO → NEXT → STRT → DONE
TODO → WAIT → NEXT
TODO → CANX
NEXT → TODO (deprioritize)
NEXT → WAIT (blocked)
WAIT → NEXT (unblocked)
```

**Always Log**:
- State changes
- Rescheduling
- Deadline changes
- Significant notes

## Examples

### Example 1: Mark as Done
User: "Mark the authentication bug fix as done"

1. Find the TODO:
```bash
grep -n "authentication bug" ~/desktop/org/todos.org
# Result: 245:** NEXT [#1] Fix authentication bug
```

2. Update:
```org
** DONE [#1] Fix authentication bug
CLOSED: [2025-12-04 Thu 15:30]
:LOGBOOK:
- State "DONE"       from "NEXT"       [2025-12-04 Thu 15:30]
:END:
```

### Example 2: Reschedule to Tomorrow
User: "Move the PR review to tomorrow"

1. Find TODO
2. Update SCHEDULED:
```org
** TODO [#2] Review upstream pull request
SCHEDULED: <2025-12-05 Fri>
:LOGBOOK:
- Rescheduled from "[2025-12-04 Thu]" on [2025-12-04 Thu 15:30]
:END:
```

### Example 3: Increase Priority
User: "Make the documentation update high priority"

1. Find TODO
2. Update priority:
```org
** TODO [#2] Update project documentation
```

### Example 4: Mark as Waiting
User: "Mark MQTT setup as waiting for hardware"

1. Find TODO
2. Change state with reason:
```org
** WAIT [#3] Setup MQTT broker on rhea
:LOGBOOK:
- State "WAIT"       from "NEXT"       [2025-12-04 Thu 15:30] \\
  Waiting for new Raspberry Pi to arrive
:END:
```

### Example 5: Add Progress Note
User: "Add a note that I completed the initial research"

1. Find TODO
2. Add note to LOGBOOK:
```org
** STRT Research alternative approaches
:LOGBOOK:
- Note taken on [2025-12-04 Thu 15:30] \\
  Completed initial research. Found three viable options.
  Next: prototype proof of concept
:END:
```

### Example 6: Cancel TODO
User: "Cancel the framework migration task, we're staying with current"

1. Find TODO
2. Mark as CANX with reason:
```org
** CANX Migrate to new framework
CLOSED: [2025-12-04 Thu 15:30]
:LOGBOOK:
- State "CANX"       from "TODO"       [2025-12-04 Thu 15:30] \\
  Team decided to continue with current framework after evaluation
:END:
```

## Batch Updates

### Mark Multiple as DONE
When completing related tasks:

```bash
# List TODOs to update
grep -n "authentication" ~/desktop/org/todos.org

# Update each one individually with proper logging
```

### Reschedule Multiple
When moving work to next week:

```bash
# Find all scheduled for this week
this_week=$(date +"%Y-%m-%d")
grep -B2 "SCHEDULED: <$this_week" ~/desktop/org/todos.org

# Update each with new dates
```

## Progress Tracking

### Update Subtask Progress
When completing subtasks, update the progress counter:

**Before**:
```org
** TODO Keyboard improvements [0/3]

*** TODO Leader keys
*** TODO Nav/media layers
*** TODO Symbol combos
```

**After completing one**:
```org
** TODO Keyboard improvements [1/3]

*** DONE Leader keys
CLOSED: [2025-12-04 Thu 15:30]
*** TODO Nav/media layers
*** TODO Symbol combos
```

### Update Recurring Tasks
For repeating tasks, mark done to advance:

**Before**:
```org
** TODO Weekly review
SCHEDULED: <2025-12-01 Mon ++1w>
:PROPERTIES:
:LAST_REPEAT: [2025-11-24 Mon 10:00]
:END:
```

**After marking done**:
```org
** TODO Weekly review
SCHEDULED: <2025-12-08 Mon ++1w>
:PROPERTIES:
:LAST_REPEAT: [2025-12-01 Mon 10:00]
:END:
```

## Validation

Before updating, verify:
- [ ] Correct TODO found
- [ ] State transition makes sense
- [ ] LOGBOOK entry added for state change
- [ ] CLOSED timestamp if completing
- [ ] Reason provided if cancelling/waiting
- [ ] Rescheduling logged if changing dates

## Tips

1. **Always log state changes**: Future you will want to know when and why
2. **Add reasons for WAIT/CANX**: Context helps decision-making
3. **Be specific with notes**: "Initial research complete" > "Progress"
4. **Update subtask counters**: Helps track project progress
5. **Reschedule honestly**: Don't keep pushing dates, maybe cancel instead
6. **Use NEXT sparingly**: Only for truly next actions
7. **Complete fully**: Mark DONE when actually done, not almost done
8. **Archive after**: Move DONE items to archive regularly

## Integration

### After Git Commit
```bash
# Mark related TODO as done
echo "Completed fix for issue #123"
# Find and mark TODO as DONE
```

### After Meeting
```bash
# Update meeting TODO, add notes
# Example: Add action items from meeting to LOGBOOK
```

### After Testing
```bash
# Update TODO with test results
# Add note about what works/doesn't work
```

## Common Mistakes to Avoid

❌ **Don't skip LOGBOOK**: Always log state changes
❌ **Don't forget CLOSED**: Add timestamp when marking DONE/CANX
❌ **Don't leave STRT**: Either complete or move back to NEXT
❌ **Don't reschedule forever**: Cancel if not getting to it
❌ **Don't use vague notes**: Be specific about progress/blockers
