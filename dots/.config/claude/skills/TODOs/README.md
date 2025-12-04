# TODO Management Skill

Comprehensive TODO management for org-mode files.

## Quick Reference

### Files
- **Main**: `~/desktop/org/todos.org` - Organized TODO list
- **Inbox**: `~/desktop/org/inbox.org` - Quick capture

### Sections in todos.org
1. **Work** - Job-related tasks
2. **Projects** - Multi-step initiatives
3. **Systems** - Homelab/infrastructure
4. **Personal** - Life admin
5. **Routines** - Recurring events
6. **Appointments** - Scheduled appointments
7. **Health** - Health tracking

### States
- **TODO** - Not started
- **NEXT** - Next action
- **STRT** - In progress
- **WAIT** - Blocked/waiting
- **DONE** - Completed
- **CANX** - Cancelled

### Priorities
- **[#1]** - Highest/most important
- **[#2]** - High
- **[#3]** - Medium
- **[#4]** - Low
- **[#5]** - Lowest

## Workflows

### [Add Workflow](workflows/Add.md)
Create new TODOs in inbox or directly to sections.

**Quick patterns**:
- Inbox capture: `echo "* TODO description" >> ~/desktop/org/inbox.org`
- Direct add: Insert to appropriate section with proper formatting
- Projects: Create with subtasks and progress tracking
- Recurring: Set up repeating schedules

### [View Workflow](workflows/View.md)
Display and filter TODOs for planning.

**Common views**:
- Active tasks (NEXT, STRT)
- Scheduled for today
- Upcoming deadlines
- By priority level
- By section (Work, Systems, etc.)
- What to work on now

### [Update Workflow](workflows/Update.md)
Modify existing TODOs: state, priority, scheduling.

**Common updates**:
- Mark as DONE with timestamp
- Reschedule to new date
- Change priority
- Add progress notes
- Mark as WAIT with reason
- Cancel with explanation

### [Refile Workflow](workflows/Refile.md)
Move TODOs from inbox to proper sections or reorganize between sections.

**Key operations**:
- Process inbox items
- Enhance with properties and context
- Move between sections as priorities change
- Batch process similar items

### [Archive Workflow](workflows/Archive.md)
Move completed and cancelled items to archive files.

**Maintenance**:
- Weekly archiving of DONE items
- Keep archive structure
- Search archived items
- Extract learnings from history

### [Review Workflow](workflows/Review.md)
Regular review processes to maintain system health.

**Review cadence**:
- Daily: Morning planning (5 min) and evening review (5 min)
- Weekly: Comprehensive review and planning (20-30 min)
- Monthly: Big picture review and cleanup (45-60 min)

### [Project Workflow](workflows/Project.md)
Manage multi-step projects with subtasks and progress tracking.

**Project features**:
- Break down into 3-7 subtasks
- Track progress [n/m]
- Sequential or parallel tasks
- Nested projects for large initiatives

### [Recurring Workflow](workflows/Recurring.md)
Set up and manage repeating tasks, habits, and meetings.

**Recurring types**:
- Fixed schedule meetings (`+1w`)
- Flexible habits (`++1d`)
- Skippable tasks (`.+1w`)
- Daily, weekly, monthly, yearly patterns

## Suggested Daily Workflows

### Morning Planning (5 minutes)
1. Check inbox - refile items
2. View scheduled for today
3. Check upcoming deadlines
4. Pick 2-3 NEXT actions
5. Mark one as STRT to begin

```bash
# What's on tap today?
today=$(date +"%Y-%m-%d")
echo "=== Today's Schedule ==="
grep -B2 "SCHEDULED: <$today" ~/desktop/org/todos.org | grep "^\*\*"

echo -e "\n=== In Progress ==="
grep "^\*\* STRT" ~/desktop/org/todos.org

echo -e "\n=== Next Actions ==="
grep "^\*\* NEXT" ~/desktop/org/todos.org | head -5
```

### End of Day Review (5 minutes)
1. Mark completed items as DONE
2. Add notes to in-progress items
3. Quick inbox check
4. Plan tomorrow's NEXT

### Weekly Review (20 minutes)
1. Archive all DONE/CANX items
2. Review all NEXT - still relevant?
3. Check stale TODOs - reschedule or cancel
4. Plan next week's priorities
5. Update project progress

## Quick Commands

### View Active Work
```bash
# What am I working on?
grep "^\*\* STRT" ~/desktop/org/todos.org

# What's next?
grep "^\*\* NEXT" ~/desktop/org/todos.org
```

### Quick Add to Inbox
```bash
# Capture quickly
echo "* TODO Review PR #123" >> ~/desktop/org/inbox.org
```

### Today's Schedule
```bash
today=$(date +"%Y-%m-%d")
grep -B2 "SCHEDULED: <$today" ~/desktop/org/todos.org | grep "^\*\*"
```

### High Priority Items
```bash
grep "^\*\* \(TODO\|NEXT\) \[#[12]\]" ~/desktop/org/todos.org
```

### Work Section Overview
```bash
sed -n '/^\* Work/,/^\* [A-Z]/p' ~/desktop/org/todos.org | \
  grep "^\*\* \(NEXT\|STRT\|TODO\)"
```

## Best Practices

### Capture
✅ Add to inbox immediately when thought occurs
✅ Don't worry about perfect formatting
✅ Refile during daily/weekly review
✅ Include links/context while fresh

### Organization
✅ Keep NEXT to 3-5 items maximum
✅ Use STRT for only current work
✅ Break large tasks into projects with subtasks
✅ Link related notes and resources

### Scheduling
✅ SCHEDULED = when to start
✅ DEADLINE = when it must be done
✅ Don't over-schedule
✅ Be realistic with dates

### Maintenance
✅ Archive completed items weekly
✅ Review and cancel stale TODOs
✅ Keep sections organized
✅ Update progress on projects

## Integration Examples

### With Git
```bash
# After commit, create follow-up
last_commit=$(git log -1 --oneline)
echo "* TODO Test changes from $last_commit" >> ~/desktop/org/inbox.org
```

### With Notes
Link between TODOs and notes:
```org
** TODO Implement new feature
From: [[file:~/desktop/org/notes/20251204--feature-design__work.org][Feature Design Note]]
```

### With Code
Reference specific code locations:
```org
** TODO Refactor authentication
See: [[file:~/src/project/src/auth/login.ts::42][login.ts:42]]
```

## Tips

1. **Inbox is your friend**: Capture everything there first
2. **Review regularly**: Daily for schedule, weekly for backlog
3. **Be honest**: Cancel TODOs you'll never do
4. **Link liberally**: Context helps future you
5. **Keep it simple**: Don't overthink the system
6. **Trust the process**: Let the system work for you
7. **Focus on NEXT**: That's what matters today
8. **Archive often**: Keep main view clean

## Common Questions

**Q: Too many TODOs?**
A: Focus on NEXT items only. Archive or cancel the rest.

**Q: How many NEXT items?**
A: 3-5 maximum. More means nothing is truly "next".

**Q: When to use STRT vs NEXT?**
A: STRT = actively working right now. NEXT = ready to start.

**Q: Inbox vs direct add?**
A: Default to inbox. Direct add when you know exactly where it goes.

**Q: How often to review?**
A: Daily: 5 min for schedule. Weekly: 20 min for full review.

**Q: What about someday/maybe?**
A: Use low priority [#5] or keep in separate file. Archive if not doing in 6 months.

## File Structure Example

```org
#+title: TODOs

* Work
:PROPERTIES:
:ARCHIVE:  archive/work::
:CATEGORY: work
:END:

** NEXT [#2] Review upstream PR
SCHEDULED: <2025-12-05 Fri>

** STRT Weekly 1:1 meeting
SCHEDULED: <2025-12-04 Thu 10:00 ++1w>

** TODO [#3] Update documentation
DEADLINE: <2025-12-10 Wed>

* Projects
:PROPERTIES:
:ARCHIVE:  archive/projects::
:END:

** TODO Keyboard improvements [1/3]

*** DONE Leader key implementation
CLOSED: [2025-12-04 Thu 15:30]

*** TODO Nav/media layer standardization
SCHEDULED: <2025-12-06 Sat>

*** TODO Symbol combos

* Systems
:PROPERTIES:
:ARCHIVE:  archive/systems::
:CATEGORY: systems
:END:

** TODO [#3] Setup MQTT on rhea
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:END:

* Personal
:PROPERTIES:
:ARCHIVE:  archive/personal::
:CATEGORY: personal
:END:

** TODO Schedule dentist appointment
DEADLINE: <2025-12-15 Mon>
```

## Getting Started

1. **Familiarize with structure**: Read todos.org, understand sections
2. **Start with inbox**: Capture a few TODOs
3. **Practice viewing**: Try different grep patterns
4. **Daily routine**: Morning planning, end of day review
5. **Weekly review**: Archive, cancel, plan ahead
6. **Iterate**: Adjust as you learn what works

## Next Steps

- Explore [Add Workflow](workflows/Add.md) for creating TODOs
- Learn [View Workflow](workflows/View.md) for finding what to work on
- Master [Update Workflow](workflows/Update.md) for tracking progress
- Set up your daily/weekly review routine
- Integrate with your notes and code workflows
