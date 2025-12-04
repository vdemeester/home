# View TODOs Workflow

## Purpose
Display and filter TODO items to help plan and prioritize work.

## When to Use
- User asks "what should I work on?"
- Show active tasks
- Review scheduled items
- Check deadlines
- Daily/weekly planning

## View Categories

### 1. Active Tasks
Tasks currently in progress or ready to start:
- State: NEXT or STRT
- Has SCHEDULED date (today or past)
- Has DEADLINE (coming up)

### 2. Scheduled for Today
Tasks scheduled for today's date.

### 3. Upcoming Deadlines
Tasks with deadlines in next 7 days.

### 4. By Priority
Filter by priority level [#1] through [#5].

### 5. By Section
View tasks from specific sections (Work, Systems, Projects, etc.).

### 6. By State
Filter by TODO, NEXT, STRT, WAIT states.

## Grep Patterns

### Show NEXT Tasks
```bash
grep -n "^\*\* NEXT" ~/desktop/org/todos.org
```

### Show STRT (In Progress) Tasks
```bash
grep -n "^\*\* STRT" ~/desktop/org/todos.org
```

### Show Today's Schedule
```bash
today=$(date +"%Y-%m-%d")
grep -B2 "SCHEDULED: <$today" ~/desktop/org/todos.org | grep "^\*\*"
```

### Show This Week's Deadlines
```bash
# Deadlines in next 7 days (requires date calculation)
for i in {0..7}; do
  day=$(date -d "+$i days" +"%Y-%m-%d")
  echo "=== $day ==="
  grep -B2 "DEADLINE: <$day" ~/desktop/org/todos.org | grep "^\*\*"
done
```

### Show by Priority
```bash
# High priority (1-2)
grep "^\*\* \(TODO\|NEXT\|STRT\) \[#[12]\]" ~/desktop/org/todos.org

# Specific priority
grep "^\*\* \(TODO\|NEXT\|STRT\) \[#2\]" ~/desktop/org/todos.org
```

### Show by Section
```bash
# Work section TODOs
sed -n '/^\* Work/,/^\* [A-Z]/p' ~/desktop/org/todos.org | \
  grep "^\*\* \(TODO\|NEXT\|STRT\)"

# Systems section TODOs
sed -n '/^\* Systems/,/^\* [A-Z]/p' ~/desktop/org/todos.org | \
  grep "^\*\* \(TODO\|NEXT\|STRT\)"

# Projects section
sed -n '/^\* Projects/,/^\* [A-Z]/p' ~/desktop/org/todos.org | \
  grep "^\*\* \(TODO\|NEXT\|STRT\)"
```

### Show Unscheduled TODOs
```bash
# TODOs without SCHEDULED or DEADLINE
awk '
  /^\*\* (TODO|NEXT)/ {
    p=1
    todo=$0
    scheduled=0
  }
  p && /SCHEDULED:|DEADLINE:/ {
    scheduled=1
  }
  p && /^$/ {
    if (!scheduled) print todo
    p=0
  }
' ~/desktop/org/todos.org
```

## Smart Filters

### What to Work On Now
Combine multiple criteria for actionable view:

```bash
# High priority NEXT tasks
grep "^\*\* NEXT \[#[12]\]" ~/desktop/org/todos.org

# Or tasks scheduled for today
today=$(date +"%Y-%m-%d")
grep -B2 "SCHEDULED: <$today" ~/desktop/org/todos.org | grep "^\*\*"

# Or upcoming deadlines (next 3 days)
for i in {0..3}; do
  day=$(date -d "+$i days" +"%Y-%m-%d")
  grep -B2 "DEADLINE: <$day" ~/desktop/org/todos.org | grep "^\*\*"
done
```

### Weekly Overview
```bash
# Count by state
echo "=== Task Summary ==="
echo "NEXT: $(grep -c '^\*\* NEXT' ~/desktop/org/todos.org)"
echo "STRT: $(grep -c '^\*\* STRT' ~/desktop/org/todos.org)"
echo "TODO: $(grep -c '^\*\* TODO' ~/desktop/org/todos.org)"
echo "WAIT: $(grep -c '^\*\* WAIT' ~/desktop/org/todos.org)"
```

### Recently Created
```bash
# Tasks created in last 7 days
week_ago=$(date -d "7 days ago" +"%Y-%m-%d")
grep -A5 "CREATED:" ~/desktop/org/todos.org | \
  grep "$week_ago\|$(date +%Y-%m-%d)" | \
  grep "CREATED:"
```

## Output Format

### Simple List
Just show the TODO lines:
```
** TODO [#2] Review upstream pull request
** NEXT Setup MQTT broker on rhea
** STRT Keyboard firmware improvements
```

### With Context
Show TODO with scheduling/deadline info:
```
** TODO [#2] Review upstream pull request
SCHEDULED: <2025-12-05 Fri>

** NEXT Setup MQTT broker on rhea
DEADLINE: <2025-12-10 Wed>
```

### With Line Numbers
For easy navigation:
```
142:** TODO [#2] Review upstream pull request
156:** NEXT Setup MQTT broker on rhea
```

### Summary Counts
For overview:
```
Work Section:
  NEXT: 3
  TODO: 12
  High Priority: 5

Systems Section:
  NEXT: 2
  TODO: 8
  High Priority: 2
```

## View Templates

### Daily Planning View
What to show for "what should I work on today?":

1. **In Progress (STRT)**
   - Show current work first

2. **Scheduled for Today**
   - Tasks with today's SCHEDULED date

3. **Urgent Deadlines**
   - Deadlines today or tomorrow

4. **High Priority NEXT**
   - NEXT tasks with [#1] or [#2]

5. **Available TODO**
   - Unscheduled high-priority TODOs

### Weekly Planning View
What to show for weekly review:

1. **This Week's Schedule**
   - All SCHEDULED items this week

2. **This Week's Deadlines**
   - All DEADLINE items this week

3. **Project Progress**
   - Active projects with subtask counts

4. **Stale TODOs**
   - Old TODOs without scheduling

### Section-Specific View
When user asks about specific area:

```bash
# Example: "Show my work todos"
section="Work"
echo "=== $section Tasks ==="
echo ""
echo "In Progress:"
sed -n "/^\* $section/,/^\* [A-Z]/p" ~/desktop/org/todos.org | \
  grep "^\*\* STRT"
echo ""
echo "Next Actions:"
sed -n "/^\* $section/,/^\* [A-Z]/p" ~/desktop/org/todos.org | \
  grep "^\*\* NEXT"
echo ""
echo "High Priority:"
sed -n "/^\* $section/,/^\* [A-Z]/p" ~/desktop/org/todos.org | \
  grep "^\*\* TODO \[#[12]\]"
```

## Interactive Examples

### Example 1: "What should I work on?"
Response:
```
Here are your active tasks:

Currently Working On (STRT):
  - Keyboard firmware improvements

Next Actions (NEXT):
  - [#2] Review upstream pull request (Work)
  - Setup MQTT broker on rhea (Systems)

Scheduled for Today:
  - Update project documentation (DEADLINE today)

High Priority TODOs:
  - [#1] Fix authentication bug (Work)
  - [#2] Configure wireguard (Systems)
```

### Example 2: "Show my work todos"
Response:
```
Work Section Tasks:

In Progress (STRT):
  - Weekly 1:1 with Manager (scheduled Mon 10:00 +1w)

Next Actions (NEXT):
  - [#2] Review upstream pull request
  - [#3] Update CLI roadmap discussion

High Priority (TODO):
  - [#1] Fix authentication bug
  - [#2] Tracability on tektoncd/pipeline

Other TODOs: 15
```

### Example 3: "What's due this week?"
Response:
```
Deadlines This Week:

Today (2025-12-04):
  - Update project documentation

Friday (2025-12-05):
  - Submit report

Monday (2025-12-08):
  - Prepare presentation slides

Scheduled This Week:
  - 8 tasks scheduled
  - 3 recurring meetings
```

## Advanced Views

### Blocked Tasks (WAIT)
```bash
echo "=== Waiting On ==="
grep -A5 "^\*\* WAIT" ~/desktop/org/todos.org
```

### Subtask Progress
```bash
# Projects with completion tracking [n/m]
grep "\[.*/..*\]" ~/desktop/org/todos.org
```

### Recently Completed
```bash
# Show recent DONE items (last 7 days)
week_ago=$(date -d "7 days ago" +"%Y-%m-%d")
grep -B2 "State \"DONE\".*\[$week_ago\|$(date +%Y-%m-%d)" ~/desktop/org/todos.org | \
  grep "^\*\*"
```

## Tips

1. **Start with NEXT**: Focus on NEXT and STRT items
2. **Check deadlines daily**: Avoid surprises
3. **Use priorities**: Filter by [#1] and [#2] when overwhelmed
4. **Limit NEXT**: Keep NEXT actions to 3-5 items
5. **Regular reviews**: Daily for schedule, weekly for backlog
6. **Context matters**: View by section for focused work

## Common Questions

**Q: "Too many TODOs, what's important?"**
A: Show NEXT + high priority + deadlines

**Q: "What can I work on now?"**
A: Show NEXT without blockers

**Q: "What's on my plate this week?"**
A: Show scheduled + deadlines for this week

**Q: "What work tasks do I have?"**
A: Show Work section, filter by active states

**Q: "What's in my inbox?"**
A: Read inbox.org, show count and recent items
