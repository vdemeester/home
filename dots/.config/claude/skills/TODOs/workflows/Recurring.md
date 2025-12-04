# Recurring TODO Workflow

## Purpose
Manage repeating tasks, habits, meetings, and maintenance routines.

## When to Use
- Regular meetings (weekly 1:1s, stand-ups)
- Maintenance tasks (backups, updates, reviews)
- Habits (exercise, journaling, reading)
- Periodic reviews (daily, weekly, monthly)
- Scheduled events (appointments, classes)

## Recurring Task Types

### 1. Fixed Schedule
Same time, same day, always:
```org
** STRT Weekly team meeting
SCHEDULED: <2025-12-05 Fri 14:00 ++1w>
:PROPERTIES:
:LAST_REPEAT: [2025-11-28 Fri 14:00]
:CATEGORY: work
:END:
```

### 2. Flexible Schedule
After completion, schedule next occurrence:
```org
** TODO Weekly review
SCHEDULED: <2025-12-08 Mon ++1w>
:PROPERTIES:
:STYLE:    habit
:LAST_REPEAT: [2025-12-01 Mon 13:40]
:END:
```

### 3. Deadline-Based
Must be done by certain interval:
```org
** TODO Monthly backup check
DEADLINE: <2025-12-31 Wed +1m>
:PROPERTIES:
:LAST_REPEAT: [2025-11-30 Sat 15:30]
:END:
```

## Repeater Syntax

### `+` Repeater (Strict)
Repeats from **original date**, even if late:

```org
SCHEDULED: <2025-12-05 Fri +1w>
```

- If you complete on Dec 5 → next is Dec 12
- If you complete on Dec 8 → next is still Dec 12
- **Use for**: Time-sensitive events, meetings

### `++` Repeater (Shift from today)
Repeats from **completion date**:

```org
SCHEDULED: <2025-12-05 Fri ++1w>
```

- If you complete on Dec 5 → next is Dec 12
- If you complete on Dec 8 → next is Dec 15
- **Use for**: Flexible tasks, maintenance, habits

### `.+` Repeater (Shift and skip)
Like `++` but doesn't nag if you skip:

```org
SCHEDULED: <2025-12-05 Fri .+1w>
```

- If you complete on Dec 5 → next is Dec 12
- If you complete on Dec 15 → next is Dec 22 (skips Dec 12)
- **Use for**: Habits you want to do but can skip

## Time Intervals

### Common Intervals
```org
+1d    Daily
+1w    Weekly
+2w    Bi-weekly
+1m    Monthly
+3m    Quarterly
+1y    Yearly
```

### Specific Days
```org
+1w Mon        Every Monday
+2w Fri        Every other Friday
+1m 1          First of each month
+1m -1         Last day of each month
```

### Complex Patterns
```org
++1w Mon Wed Fri    Mon/Wed/Fri after completion
+1m 1 15            1st and 15th of each month
```

## Habit Tracking

### Basic Habit
```org
** TODO Daily exercise
SCHEDULED: <2025-12-05 Fri ++1d>
:PROPERTIES:
:STYLE:    habit
:LAST_REPEAT: [2025-12-04 Thu 07:00]
:END:
```

### Habit with Consistency Goal
```org
** TODO Morning journaling
SCHEDULED: <2025-12-05 Fri .+1d>
:PROPERTIES:
:STYLE:    habit
:LAST_REPEAT: [2025-12-04 Thu 06:30]
:HABIT_CONSISTENCY: 5d
:END:

Goal: Journal at least 5 days per week
```

## Common Recurring Patterns

### Weekly Meeting
```org
** STRT 1:1 with Manager
SCHEDULED: <2025-12-05 Fri 10:00 +1w>
:PROPERTIES:
:LAST_REPEAT: [2025-11-28 Fri 10:18]
:CATEGORY: work
:END:

Meeting notes: [[file:~/notes/meeting-notes.org][Meeting Notes]]
```

### Daily Standup
```org
** STRT Daily standup
SCHEDULED: <2025-12-05 Fri 09:00 +1d Mon Tue Wed Thu Fri>
:PROPERTIES:
:LAST_REPEAT: [2025-12-04 Thu 09:00]
:CATEGORY: work
:END:

Team sync - what did I do, what am I doing, any blockers
```

### Weekly Review
```org
** TODO Weekly planning and review
SCHEDULED: <2025-12-08 Mon 09:00 ++1w>
:PROPERTIES:
:STYLE:    habit
:LAST_REPEAT: [2025-12-01 Mon 09:15]
:END:

- Review last week
- Plan this week
- Archive completed items
```

### Monthly Maintenance
```org
** TODO Server maintenance and updates
SCHEDULED: <2025-12-15 Mon ++1m>
:PROPERTIES:
:LAST_REPEAT: [2025-11-15 Sun 14:30]
:CATEGORY: systems
:END:

- Update packages
- Check backups
- Review logs
- Security patches
```

### Quarterly Review
```org
** TODO Quarterly goal review
DEADLINE: <2025-12-31 Wed +3m>
:PROPERTIES:
:LAST_REPEAT: [2025-09-30 Mon 15:00]
:END:

Review progress on yearly goals and adjust
```

### Annual Tasks
```org
** TODO Annual tax preparation
DEADLINE: <2025-04-15 Wed +1y>
:PROPERTIES:
:LAST_REPEAT: [2024-04-10 Mon 20:00]
:END:

Gather documents and file taxes
```

## Managing Recurring Tasks

### Completing a Recurring Task

When you mark DONE:
```org
# Before completion
** TODO Weekly review
SCHEDULED: <2025-12-08 Mon ++1w>
:PROPERTIES:
:LAST_REPEAT: [2025-12-01 Mon 13:40]
:END:

# After marking DONE on Dec 8
** TODO Weekly review
SCHEDULED: <2025-12-15 Mon ++1w>
:PROPERTIES:
:LAST_REPEAT: [2025-12-08 Mon 14:15]
:END:
:LOGBOOK:
- State "DONE"       from "TODO"       [2025-12-08 Mon 14:15]
:END:
```

**What happens**:
- State returns to TODO (or original state)
- SCHEDULED advances by interval
- LAST_REPEAT updated to completion time
- LOGBOOK entry added

### Skipping an Occurrence

**Option 1**: Mark DONE anyway
- Advances schedule
- Records you acknowledged it

**Option 2**: Reschedule to next occurrence
```org
# Manually update SCHEDULED to next date
SCHEDULED: <2025-12-15 Mon ++1w>
```

**Option 3**: Add note explaining skip
```org
:LOGBOOK:
- Note taken on [2025-12-08 Mon 10:00] \\
  Skipped this week due to holiday
:END:
```

### Stopping a Recurring Task

**Mark as CANX**:
```org
** CANX Weekly team sync (team disbanded)
CLOSED: [2025-12-08 Mon 10:00]
:LOGBOOK:
- State "CANX"       from "STRT"       [2025-12-08 Mon 10:00] \\
  Team disbanded, meeting no longer needed
:END:
```

**Remove repeater** (make one-time):
```org
# Before
SCHEDULED: <2025-12-08 Mon ++1w>

# After
SCHEDULED: <2025-12-08 Mon>
```

### Changing Frequency

**Update repeater interval**:
```org
# Was weekly
SCHEDULED: <2025-12-08 Mon ++1w>

# Now bi-weekly
SCHEDULED: <2025-12-08 Mon ++2w>
```

**Update day/time**:
```org
# Was Friday 14:00
SCHEDULED: <2025-12-05 Fri 14:00 +1w>

# Now Monday 10:00
SCHEDULED: <2025-12-08 Mon 10:00 +1w>
```

## Recurring Task Templates

### Daily Habit
```org
** TODO [Habit name]
SCHEDULED: <YYYY-MM-DD Day .+1d>
:PROPERTIES:
:STYLE:    habit
:LAST_REPEAT: [previous-completion]
:END:

[Description and notes]
```

### Weekly Task
```org
** TODO [Task name]
SCHEDULED: <YYYY-MM-DD Day ++1w>
:PROPERTIES:
:LAST_REPEAT: [previous-completion]
:CATEGORY: [category]
:END:

[Context and links]
```

### Monthly Maintenance
```org
** TODO [Maintenance task]
SCHEDULED: <YYYY-MM-DD Day ++1m>
:PROPERTIES:
:LAST_REPEAT: [previous-completion]
:CATEGORY: systems
:END:

Checklist:
- [ ] Item 1
- [ ] Item 2
- [ ] Item 3
```

### Meeting with Notes
```org
** STRT [Meeting name]
SCHEDULED: <YYYY-MM-DD Day HH:MM +1w>
:PROPERTIES:
:LAST_REPEAT: [previous-occurrence]
:CATEGORY: work
:END:

Agenda:
- Topic 1
- Topic 2

Notes: [[file:~/notes/meeting-notes.org::*Meeting Name][Meeting Notes]]
```

## Tracking Habits

### Simple Habit Tracking
Just mark DONE each time:
```org
** TODO Morning exercise
SCHEDULED: <2025-12-05 Fri .+1d>
:PROPERTIES:
:STYLE:    habit
:LAST_REPEAT: [2025-12-04 Thu 07:15]
:END:
```

Check LOGBOOK for consistency.

### Habit with Notes
Track details:
```org
** TODO Meditation
SCHEDULED: <2025-12-05 Fri .+1d>
:PROPERTIES:
:STYLE:    habit
:LAST_REPEAT: [2025-12-04 Thu 06:30]
:END:
:LOGBOOK:
- State "DONE"       from "TODO"       [2025-12-04 Thu 06:30] \\
  15 minutes, felt focused
- State "DONE"       from "TODO"       [2025-12-03 Wed 06:45] \\
  10 minutes, distracted
:END:
```

### Habit Streaks
Check consistency:
```bash
# See completion pattern
grep "State \"DONE\"" todos.org | \
  grep "Morning exercise" | \
  tail -14
```

## Tips

1. **Choose right repeater**:
   - Fixed schedule? Use `+`
   - Flexible? Use `++`
   - Can skip? Use `.+`

2. **Start with STRT or TODO**:
   - STRT for ongoing (meetings)
   - TODO for tasks (reviews, maintenance)

3. **Track in LOGBOOK**: Notes about each occurrence

4. **Use LAST_REPEAT**: See when last completed

5. **Keep in Routines section**: Unless specific to other section

6. **Link to resources**: Meeting notes, checklists, docs

7. **Review regularly**: Are these still needed?

8. **Adjust frequency**: If consistently late, maybe less frequent

## Viewing Recurring Tasks

### All Recurring Tasks
```bash
grep -B2 "SCHEDULED:.*+\|DEADLINE:.*+" ~/desktop/org/todos.org | \
  grep "^\*\*"
```

### Today's Recurring Items
```bash
today=$(date +"%Y-%m-%d")
grep -B2 "SCHEDULED: <$today.*+" ~/desktop/org/todos.org | \
  grep "^\*\*"
```

### Habit Consistency
```bash
# Check last 10 completions of a habit
grep -A3 "^\*\* TODO Morning exercise" ~/desktop/org/todos.org | \
  grep "State \"DONE\"" | \
  tail -10
```

## Common Issues

### Missed Several Occurrences
```org
# Task scheduled for Dec 1, today is Dec 15
SCHEDULED: <2025-12-01 Mon ++1w>
```

**Solution**: Mark DONE to advance to next
- If `++1w`: advances to Dec 22 (week from completion)
- If `+1w`: would go to next scheduled (Dec 8, 15, 22...)

### Wrong Interval
```org
# Was daily but should be weekly
SCHEDULED: <2025-12-05 Fri ++1d>
```

**Solution**: Update interval
```org
SCHEDULED: <2025-12-05 Fri ++1w>
```

### No Longer Needed
```org
** STRT Old recurring meeting
SCHEDULED: <2025-12-05 Fri +1w>
```

**Solution**: Mark as CANX with reason
```org
** CANX Old recurring meeting
CLOSED: [2025-12-05 Fri 10:00]
:LOGBOOK:
- State "CANX"       from "STRT"       [2025-12-05 Fri 10:00] \\
  Project completed, meeting no longer needed
:END:
```

## Integration

### With Calendar
Recurring tasks show up on schedule in org-agenda.

### With Notes
Link to meeting notes or habit journal:
```org
** STRT Weekly 1:1
SCHEDULED: <2025-12-05 Fri 10:00 +1w>

Notes: [[file:~/desktop/org/notes/20251204--1on1-notes__work.org][1:1 Meeting Notes]]
```

### With Projects
Track recurring reviews of projects:
```org
** TODO Review project X progress
SCHEDULED: <2025-12-08 Mon ++1w>

Project: [[*Project X][Project X Details]]
```

## Anti-Patterns

❌ **Too many recurring tasks**: Overwhelming, won't complete
❌ **Wrong repeater type**: Using `+` when should use `++`
❌ **Never completing**: If consistently skip, maybe not needed
❌ **No notes**: Can't remember what happened each time
❌ **Outdated tasks**: Keep running things no longer relevant
✅ **Right amount**: Only essential recurring tasks
✅ **Appropriate interval**: Matches actual need
✅ **Complete regularly**: Build the habit
✅ **Track progress**: Add notes each completion
✅ **Review periodically**: Cancel what's no longer needed
