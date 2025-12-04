# Review TODO Workflow

## Purpose
Regular review processes to maintain a healthy TODO system and stay on top of commitments.

## When to Use
- Daily: Morning planning and evening review (10 minutes total)
- Weekly: Comprehensive review and planning (20-30 minutes)
- Monthly: Big picture review and cleanup (45-60 minutes)

## Daily Review

### Morning Planning (5 minutes)

**Goal**: Know what to work on today

#### 1. Check Inbox
```bash
# How many items?
grep -c "^\*" ~/desktop/org/inbox.org

# Quick scan
head -20 ~/desktop/org/inbox.org
```

**Action**: If >5 items, do quick refile

#### 2. View Today's Schedule
```bash
today=$(date +"%Y-%m-%d")
echo "=== Scheduled for Today ==="
grep -B2 "SCHEDULED: <$today" ~/desktop/org/todos.org | grep "^\*\*"
```

**Action**: Review and confirm still relevant

#### 3. Check Deadlines
```bash
echo "=== Due Today or Soon ==="
for i in {0..2}; do
  day=$(date -d "+$i days" +"%Y-%m-%d")
  result=$(grep -B2 "DEADLINE: <$day" ~/desktop/org/todos.org | grep "^\*\*")
  if [ -n "$result" ]; then
    echo "[$day]"
    echo "$result"
  fi
done
```

**Action**: Prioritize deadline items

#### 4. Review NEXT Actions
```bash
echo "=== Available Next Actions ==="
grep "^\*\* NEXT" ~/desktop/org/todos.org | head -10
```

**Action**: Pick 2-3 to focus on today

#### 5. Set Today's Intention
- What 1-3 things MUST get done?
- Mark one as STRT to begin with
- Leave others as NEXT

**Template**:
```
Today's Focus (Dec 4):
1. [STRT] Fix authentication bug
2. [NEXT] Review upstream PR #123
3. [NEXT] Update roadmap discussion
```

### Evening Review (5 minutes)

**Goal**: Capture progress and plan tomorrow

#### 1. Check What's STRT
```bash
echo "=== Currently In Progress ==="
grep "^\*\* STRT" ~/desktop/org/todos.org
```

**Action**:
- Mark as DONE if completed
- Add progress note if still working
- Move back to NEXT if blocked

#### 2. Capture New Items
```bash
# Any new TODOs from today?
echo "* TODO [thing that came up]" >> ~/desktop/org/inbox.org
```

**Action**: Quick capture anything from email, meetings, discussions

#### 3. Update Progress
Add notes to in-progress items:
```org
:LOGBOOK:
- Note taken on [2025-12-04 Thu 17:30] \\
  Completed analysis phase. Ready to implement tomorrow.
:END:
```

#### 4. Tomorrow's Setup
- Anything to schedule for tomorrow?
- Any urgent items to mark as NEXT?
- Quick scan of tomorrow's calendar

## Weekly Review

### Time: Sunday evening or Monday morning (20-30 minutes)

**Goal**: Clean system, clarify priorities, plan the week

### 1. Archive Completed Items (5 min)
```bash
# Find items to archive (older than 7 days)
week_ago=$(date -d "7 days ago" +"%Y-%m-%d")
echo "=== Completed Last Week ==="
grep -B5 "CLOSED:.*\[$week_ago" ~/desktop/org/todos.org | \
  grep "^** \(DONE\|CANX\)" | wc -l
```

**Action**: Archive all DONE/CANX items from last week

### 2. Process Inbox (5 min)
```bash
# Empty the inbox
cat ~/desktop/org/inbox.org
```

**Action**: Refile all items to appropriate sections

### 3. Review All NEXT Items (5 min)
```bash
echo "=== All NEXT Actions ==="
grep "^\*\* NEXT" ~/desktop/org/todos.org
```

**Action**:
- Still relevant? Keep as NEXT
- No longer next? Move to TODO
- Cancelled? Mark CANX
- **Goal**: Keep NEXT to 5-7 items max

### 4. Check for Stale TODOs (5 min)
```bash
# Items created >30 days ago, never scheduled
awk '/^\*\* TODO/ {
  p=1
  todo=$0
  scheduled=0
}
p && /CREATED:.*\[2025-10/ {
  old=1
}
p && /SCHEDULED:|DEADLINE:/ {
  scheduled=1
}
p && /^$/ {
  if (!scheduled && old) print todo
  p=0
  old=0
}' ~/desktop/org/todos.org
```

**Action**:
- Still want to do? Schedule it
- Not doing? Cancel it
- Unclear? Keep but review next week

### 5. Review Projects (5 min)
```bash
echo "=== Active Projects ==="
sed -n '/^\* Projects/,/^\* Systems/p' ~/desktop/org/todos.org | \
  grep "^\*\* \(TODO\|NEXT\|STRT\)" | head -10
```

**Action**:
- Update progress counters [n/m]
- Mark completed subtasks as DONE
- Archive completed projects
- Add next subtasks if needed

### 6. Check This Week's Schedule (5 min)
```bash
echo "=== This Week's Schedule ==="
for i in {0..7}; do
  day=$(date -d "+$i days" +"%Y-%m-%d %a")
  echo "=== $day ==="
  grep -B2 "SCHEDULED: <$(echo $day | cut -d' ' -f1)" ~/desktop/org/todos.org | \
    grep "^\*\*" | head -5
done
```

**Action**: Verify schedule is realistic

### 7. Plan Next Week's Priorities
- What are the top 3 priorities?
- Which projects to advance?
- Any deadlines coming up?
- Set SCHEDULED dates for priority items

**Template**:
```
Week of Dec 2-8 Priorities:
1. Complete authentication refactor
2. Review 3 upstream PRs
3. Setup MQTT on rhea

Projects Focus:
- Keyboard improvements: Leader keys implementation
- Home automation: MQTT broker setup
```

## Monthly Review

### Time: Last Sunday of month (45-60 minutes)

**Goal**: Big picture review, system maintenance, learning

### 1. Review Accomplishments (10 min)
```bash
# What got done this month?
this_month=$(date +"%Y-%m")
echo "=== Completed This Month ==="
grep -r "CLOSED: \[$this_month" ~/desktop/org/archive/*.org | wc -l

# Show details
grep -r "CLOSED: \[$this_month" ~/desktop/org/archive/*.org | \
  grep "^**" | head -20
```

**Questions**:
- What did I accomplish?
- What patterns do I see?
- What am I proud of?

### 2. Review Cancelled Items (5 min)
```bash
echo "=== Cancelled This Month ==="
grep -r "^** CANX" ~/desktop/org/archive/*.org | \
  grep "CLOSED: \[$this_month" | head -10
```

**Questions**:
- Why were these cancelled?
- Should I have said no earlier?
- Any patterns to avoid?

### 3. Review Active TODOs (10 min)
```bash
echo "=== TODO Counts by Section ==="
for section in Work Projects Systems Personal; do
  count=$(sed -n "/^\* $section/,/^\* [A-Z]/p" ~/desktop/org/todos.org | \
    grep -c "^\*\* TODO")
  echo "$section: $count TODOs"
done
```

**Action**:
- Too many in one section? Prioritize or cancel
- Any old TODOs to cancel?
- Redistribute as needed

### 4. Review Projects (10 min)
```bash
# All projects with progress
sed -n '/^\* Projects/,/^\* Systems/p' ~/desktop/org/todos.org | \
  grep "\[.*/..*\]"
```

**Questions**:
- Which projects progressed?
- Which stalled?
- Any to cancel?
- Any to start?

### 5. System Health Check (10 min)

**Check**:
- Inbox reasonable size? (<10 items)
- NEXT actions limited? (3-7 items)
- Sections organized?
- Archives working?
- Properties consistent?

**Action**: Fix any issues found

### 6. Extract Learnings (5 min)

**Questions**:
- What worked well this month?
- What didn't work?
- Any system improvements needed?
- Time estimates accurate?
- Priorities aligned with goals?

**Action**: Create note with learnings

### 7. Plan Next Month (10 min)

**Consider**:
- Major deadlines?
- Projects to focus on?
- Areas to improve?
- Anything to delegate/defer?

**Template**:
```org
* December 2025 Planning

** Focus Areas
- [ ] Complete keyboard firmware improvements
- [ ] Setup home automation basics
- [ ] Upstream OSP roadmap work

** Projects to Start
- [ ] Personal finance tracking
- [ ] Blog post about NixOS

** Experiments
- Try time-blocking for deep work
- Limit NEXT to 5 items max

** Goals
- Ship 2 major features
- Review 10 upstream PRs
- Write 1 blog post
```

## Review Checklists

### Daily Morning Checklist
- [ ] Check inbox count
- [ ] View today's schedule
- [ ] Check upcoming deadlines (3 days)
- [ ] Review NEXT actions
- [ ] Pick 1-3 focus items
- [ ] Mark one as STRT

### Daily Evening Checklist
- [ ] Update STRT items (DONE or progress note)
- [ ] Capture new items to inbox
- [ ] Quick tomorrow scan
- [ ] Celebrate today's wins

### Weekly Review Checklist
- [ ] Archive completed items (>7 days old)
- [ ] Process entire inbox
- [ ] Review all NEXT (keep to 5-7)
- [ ] Cancel or schedule stale TODOs
- [ ] Update project progress
- [ ] Review this week's schedule
- [ ] Plan next week's priorities

### Monthly Review Checklist
- [ ] Review month's accomplishments
- [ ] Analyze cancelled items
- [ ] Check TODO counts per section
- [ ] Review project progress
- [ ] System health check
- [ ] Extract learnings
- [ ] Plan next month

## Review Habits

### Make it Easy
- **Same time**: Consistency builds habit
- **Same place**: Dedicated review location
- **Same tools**: Keep scripts handy
- **Quick access**: Bookmarks, aliases

### Make it Valuable
- **Track insights**: What did you learn?
- **Celebrate wins**: Acknowledge accomplishments
- **Improve system**: Fix friction points
- **Stay honest**: Cancel what you won't do

### Make it Stick
- **Start small**: 5-min daily before 30-min weekly
- **Link to existing habit**: After morning coffee
- **Set reminder**: Calendar event
- **Review the review**: Monthly check if process works

## Metrics to Track

### Weekly
- TODOs completed
- NEXT actions count
- Inbox size
- Items archived

### Monthly
- Completion rate (done vs created)
- Average TODO age
- Projects progressed
- Cancelled percentage

## Tips

1. **Schedule reviews**: Make them non-negotiable appointments
2. **Batch process**: Faster than continuous micro-reviews
3. **Be ruthless**: Cancel liberally
4. **Celebrate wins**: Acknowledge what you completed
5. **Learn continuously**: Each review improves the system
6. **Keep it simple**: Don't over-engineer
7. **Trust the system**: Capture and let it go
8. **Stay honest**: System only works if you use it

## Integration with Other Workflows

### After Review, Take Action
- **Add workflow**: Create TODOs from review insights
- **Update workflow**: Reschedule or reprioritize
- **Archive workflow**: Clean up completed items
- **Refile workflow**: Process inbox

### Link to Notes
Create review notes:
```org
#+title:      Weekly Review 2025-W49
#+date:       [2025-12-08 Mon 09:00]
#+filetags:   :review:planning:
#+identifier: 20251208T090000
#+category: planning

* Accomplishments
- Completed keyboard firmware improvements
- Reviewed 3 upstream PRs

* Insights
- Need to limit NEXT to 5 items
- Projects stalled without scheduling

* Next Week Focus
- [[file:~/desktop/org/todos.org::*Setup MQTT broker][Setup MQTT on rhea]]
- [[file:~/desktop/org/todos.org::*Review roadmap][Update upstream roadmaps]]
```

## Common Pitfalls

❌ **Skipping reviews**: System degrades quickly
❌ **Too long**: Reviews shouldn't take hours
❌ **Not actionable**: Review should lead to decisions
❌ **No followthrough**: Plans mean nothing without action
❌ **Over-planning**: Don't schedule every minute
✅ **Consistent**: Better imperfect weekly than perfect never
✅ **Focused**: Clear outcome for each review
✅ **Honest**: Cancel what you won't do
✅ **Actionable**: Every review leads to next steps
✅ **Sustainable**: Find rhythm that works long-term
