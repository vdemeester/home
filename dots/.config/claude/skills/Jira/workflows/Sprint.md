# Sprint Management

Manage sprints, sprint planning, and sprint-related issues.

## When to Use

- User mentions "sprint", "current sprint", "sprint planning"
- Need to view sprint contents or progress
- Adding/removing issues from sprints
- Sprint planning and organization
- Sprint retrospectives or reviews

## Steps

1. **Identify sprint operation** (list, add, remove, view)
2. **Determine sprint context** (current, specific sprint, future)
3. **Execute sprint command** with appropriate options
4. **Present results** clearly
5. **Offer follow-up actions** (add issues, view details, etc.)

## Commands

### List Sprints

#### Current Sprint
```bash
jira sprint list --current
```

#### Previous Sprints
```bash
jira sprint list --prev
```

#### Future Sprints
```bash
jira sprint list --next
```

#### All Sprints
```bash
jira sprint list
```

#### Sprints for Specific Board
```bash
jira sprint list --board "Board Name"
```

### View Sprint Issues

#### Issues in Current Sprint
```bash
jira issue list --jql "sprint in openSprints()"
```

#### Issues in Specific Sprint (by ID)
```bash
jira sprint list --current --plain  # Get sprint ID
jira issue list --jql "sprint = SPRINT-ID"
```

#### My Issues in Current Sprint
```bash
jira issue list --jql "sprint in openSprints() AND assignee = currentUser()"
```

#### Sprint Issues by Status
```bash
jira issue list --jql "sprint in openSprints() ORDER BY status, priority DESC"
```

### Add Issues to Sprint

#### Add Single Issue
```bash
jira sprint add SPRINT-ID ISSUE-KEY
```

#### Add Multiple Issues
```bash
jira sprint add SPRINT-ID ISSUE-KEY-1 ISSUE-KEY-2 ISSUE-KEY-3
```

#### Add from JQL Query
```bash
# Get issues first
jira issue list --jql "priority = High AND status = 'To Do' AND sprint is EMPTY" --plain

# Then add them (in loop)
for key in ISSUE-1 ISSUE-2; do
  jira sprint add SPRINT-ID "$key"
done
```

### Remove Issues from Sprint

#### Remove Single Issue
```bash
jira sprint remove SPRINT-ID ISSUE-KEY
```

#### Remove Multiple Issues
```bash
jira sprint remove SPRINT-ID ISSUE-KEY-1 ISSUE-KEY-2
```

## Sprint Information

### Get Current Sprint ID
```bash
# List current sprint and extract ID
jira sprint list --current --plain
```

### Sprint State
Sprints can be:
- **Active/Open**: Currently running
- **Future**: Planned but not started
- **Closed**: Completed

### Sprint Metadata
When viewing sprint:
- Sprint name
- Start date
- End date
- State (active/future/closed)
- Associated board

## Common Sprint Queries

### Sprint Planning

#### High Priority Items Not in Sprint
```bash
jira issue list --jql "project = SRVKP AND priority IN (Critical, High) AND status = 'To Do' AND sprint is EMPTY ORDER BY priority DESC"
```

#### Ready for Sprint (All To Do, no sprint)
```bash
jira issue list --jql "status = 'To Do' AND sprint is EMPTY ORDER BY priority DESC"
```

#### Estimate Sprint Capacity
```bash
# List current sprint issues with story points (if configured)
jira issue list --jql "sprint in openSprints()" --plain
```

### Sprint Monitoring

#### Sprint Progress by Status
```bash
jira issue list --jql "sprint in openSprints()" --columns STATUS,COUNT --plain | \
  sort | uniq -c
```

#### Sprint Blockers
```bash
jira issue list --jql "sprint in openSprints() AND status = Blocked ORDER BY priority DESC"
```

#### Incomplete Sprint Items
```bash
jira issue list --jql "sprint in openSprints() AND status != Done"
```

#### Completed This Sprint
```bash
jira issue list --jql "sprint in openSprints() AND status = Done ORDER BY resolved DESC"
```

### Sprint Velocity

#### Issues Completed in Sprint
```bash
jira issue list --jql "sprint in openSprints() AND status = Done" --plain | wc -l
```

#### Issues Completed Last Sprint
```bash
jira issue list --jql "sprint in closedSprints() ORDER BY sprint DESC" --plain | head -20 | wc -l
```

## Examples

### Example 1: View Current Sprint
**User**: "What's in the current sprint?"

**Action**:
```bash
# Get current sprint info
jira sprint list --current

# Get issues in current sprint
jira issue list --jql "sprint in openSprints() ORDER BY status, priority DESC"
```

**Presentation**:
- Sprint name and dates
- Total issues
- Breakdown by status (To Do, In Progress, Done)
- Highlight blockers
- Show high priority items

### Example 2: Sprint Planning
**User**: "Help me plan the next sprint"

**Actions**:
1. View future sprint:
```bash
jira sprint list --next
```

2. Find high priority unassigned items:
```bash
jira issue list --jql "priority IN (Critical, High) AND status = 'To Do' AND sprint is EMPTY AND assignee = currentUser() ORDER BY priority DESC" --limit 20
```

3. Show candidate issues for sprint

**Follow-up**: "Would you like me to add any of these to the sprint?"

### Example 3: Add Issues to Sprint
**User**: "Add SRVKP-1234 and SRVKP-5678 to the current sprint"

**Actions**:
```bash
# Get current sprint ID
SPRINT_ID=$(jira sprint list --current --plain | awk '{print $1}' | head -1)

# Add issues
jira sprint add "$SPRINT_ID" SRVKP-1234
jira sprint add "$SPRINT_ID" SRVKP-5678

# Verify
jira issue view SRVKP-1234 --plain | grep -i sprint
jira issue view SRVKP-5678 --plain | grep -i sprint
```

**Response**: "Added SRVKP-1234 and SRVKP-5678 to sprint [NAME]"

### Example 4: Sprint Standup
**User**: "What's my sprint progress?"

**Actions**:
```bash
# My sprint items by status
jira issue list --jql "sprint in openSprints() AND assignee = currentUser() ORDER BY status, priority DESC"
```

**Presentation**:
- Done: X issues
- In Progress: Y issues
- To Do: Z issues
- Blocked: W issues (if any)

### Example 5: Sprint Cleanup
**User**: "Move incomplete items to next sprint"

**Actions**:
1. Get current sprint ID:
```bash
CURRENT=$(jira sprint list --current --plain | awk '{print $1}' | head -1)
```

2. Get next sprint ID:
```bash
NEXT=$(jira sprint list --next --plain | awk '{print $1}' | head -1)
```

3. Find incomplete items:
```bash
jira issue list --jql "sprint = $CURRENT AND status != Done" --plain
```

4. Move to next sprint:
```bash
jira issue list --jql "sprint = $CURRENT AND status != Done" --plain | \
while read key rest; do
  jira sprint remove "$CURRENT" "$key"
  jira sprint add "$NEXT" "$key"
  echo "Moved $key to next sprint"
done
```

### Example 6: Sprint Retrospective Data
**User**: "Show me sprint statistics for the retrospective"

**Actions**:
```bash
# Get last closed sprint
SPRINT_ID=$(jira sprint list --prev --plain | head -1 | awk '{print $1}')

# Total issues
TOTAL=$(jira issue list --jql "sprint = $SPRINT_ID" --plain | wc -l)

# Completed
DONE=$(jira issue list --jql "sprint = $SPRINT_ID AND status = Done" --plain | wc -l)

# Incomplete
INCOMPLETE=$(jira issue list --jql "sprint = $SPRINT_ID AND status != Done" --plain | wc -l)

# By type
echo "Bugs: $(jira issue list --jql "sprint = $SPRINT_ID AND type = Bug" --plain | wc -l)"
echo "Tasks: $(jira issue list --jql "sprint = $SPRINT_ID AND type = Task" --plain | wc -l)"
echo "Stories: $(jira issue list --jql "sprint = $SPRINT_ID AND type = Story" --plain | wc -l)"
```

**Presentation**:
- Sprint name and dates
- Completion rate (Done/Total)
- Issues by type
- Average time to complete
- Blockers encountered

## Sprint Planning Workflow

### 1. Review Previous Sprint
```bash
# Last sprint stats
jira sprint list --prev

# What was completed
jira issue list --jql "sprint in closedSprints() ORDER BY sprint DESC" --limit 20

# What was carried over
jira issue list --jql "sprint in closedSprints() AND status != Done ORDER BY sprint DESC"
```

### 2. Identify Capacity
- Team size
- Available days
- Planned time off
- Known commitments

### 3. Prioritize Backlog
```bash
# High priority items
jira issue list --jql "priority IN (Critical, High) AND status = 'To Do' AND sprint is EMPTY ORDER BY priority DESC, created ASC"

# Customer reported issues
jira issue list --jql "labels = customer-reported AND status = 'To Do' AND sprint is EMPTY ORDER BY priority DESC"

# Technical debt
jira issue list --jql "labels = tech-debt AND status = 'To Do' AND sprint is EMPTY"
```

### 4. Add to Sprint
```bash
# Get next/current sprint ID
SPRINT_ID=$(jira sprint list --current --plain | awk '{print $1}' | head -1)

# Add selected issues
jira sprint add "$SPRINT_ID" ISSUE-1 ISSUE-2 ISSUE-3
```

### 5. Balance Sprint
```bash
# Review sprint composition
jira issue list --jql "sprint in openSprints()" --columns TYPE,PRIORITY,SUMMARY

# Check for:
- Mix of bugs and features
- Range of priorities
- Team member distribution
- Dependencies
```

## Sprint Monitoring

### Daily

#### Daily Standup Data
```bash
# Team sprint progress
jira issue list --jql "sprint in openSprints() ORDER BY status, assignee"

# Blockers
jira issue list --jql "sprint in openSprints() AND status = Blocked"

# Recently completed
jira issue list --jql "sprint in openSprints() AND status changed to Done DURING (-1d, now())"
```

#### Burndown Tracking
```bash
# Remaining items
jira issue list --jql "sprint in openSprints() AND status != Done" --plain | wc -l

# Ideal vs actual (manual calculation based on sprint duration)
```

### Mid-Sprint

#### Health Check
```bash
# On track items
jira issue list --jql "sprint in openSprints() AND status IN ('In Progress', 'Code Review', 'QE Review')"

# At risk (still To Do with less than 3 days left)
jira issue list --jql "sprint in openSprints() AND status = 'To Do'"

# Completed so far
jira issue list --jql "sprint in openSprints() AND status = Done" --plain | wc -l
```

### End of Sprint

#### Sprint Completion
```bash
# What's done
jira issue list --jql "sprint in openSprints() AND status = Done"

# What's not done
jira issue list --jql "sprint in openSprints() AND status != Done"

# Carry over to next sprint
# (see Example 5 above)
```

## Integration Patterns

### With TODOs Skill

Create sprint TODOs in org-mode:
```bash
# Get sprint issues
jira issue list --jql "sprint in openSprints() AND assignee = currentUser()" --plain

# Create TODOs in org file for each issue
# (Use TODOs skill to create entries)
```

### With Notes Skill

Sprint planning note:
```org
* Sprint Planning - [Sprint Name]

** Sprint Goals
- Goal 1
- Goal 2

** Committed Issues
- [ ] SRVKP-1234: Issue summary
- [ ] SRVKP-5678: Issue summary

** Capacity
- Team size: X
- Days available: Y
- Planned velocity: Z points

** Risks
- Risk 1
- Risk 2
```

Sprint retrospective note:
```org
* Sprint Retrospective - [Sprint Name]

** What Went Well
- Item 1
- Item 2

** What Could Be Improved
- Item 1
- Item 2

** Action Items
- [ ] Action 1 - SRVKP-XXXX
- [ ] Action 2 - SRVKP-YYYY

** Metrics
- Planned: X issues
- Completed: Y issues
- Completion rate: Z%
```

## Best Practices

### 1. Sprint Planning
- Plan realistic capacity
- Include mix of types (bugs, features, tech debt)
- Account for dependencies
- Leave buffer for urgent items
- Balance across team members

### 2. During Sprint
- Daily standup updates
- Move items through workflow
- Flag blockers immediately
- Add unplanned work explicitly
- Update status regularly

### 3. Sprint Review
- Demo completed work
- Accept/reject stories
- Document learnings
- Update velocity data

### 4. Sprint Retrospective
- Celebrate wins
- Identify improvements
- Create action items
- Track action item completion

### 5. Continuous Improvement
- Track velocity trends
- Monitor completion rates
- Identify recurring blockers
- Adjust planning based on data

## Common Patterns

### Sprint Handoff
When transitioning sprints:
1. Close current sprint
2. Move incomplete items to backlog or next sprint
3. Create next sprint
4. Plan next sprint content
5. Start new sprint

### Emergency Additions
When urgent work arises:
1. Assess impact on sprint goals
2. Remove equal capacity if needed
3. Add to sprint
4. Document in retrospective

### Scope Creep Prevention
- Clear sprint goals
- Formal change process
- Track unplanned work
- Discuss in retrospective

## Tips

- **Use sprint goals**: Clear objectives for each sprint
- **Track velocity**: Historical data for planning
- **Visualize progress**: Charts and graphs
- **Regular updates**: Keep sprint board current
- **Team collaboration**: Involve whole team in planning
- **Protect sprint**: Minimize mid-sprint changes
- **Retrospect**: Learn from each sprint
- **Celebrate**: Acknowledge completed work

## Troubleshooting

### Can't Find Sprint
- Check board name
- Verify sprint state (active/future/closed)
- Use web UI to confirm sprint exists

### Issues Not Showing in Sprint
- Verify sprint assignment
- Check project/board configuration
- Refresh board in web UI

### Can't Add Issues to Sprint
- Verify permissions
- Check issue state (resolved issues can't be added)
- Ensure sprint is active or future

## Follow-up Actions

After sprint operations:
- **Update team**: Notify of sprint changes
- **Create notes**: Document planning decisions
- **Update TODOs**: Sync personal task list
- **Review dependencies**: Check linked issues
- **Plan ceremonies**: Schedule sprint events

## Related Commands

```bash
# Board operations
jira board list

# Issue operations
jira issue list --jql "sprint = SPRINT-ID"
jira issue edit ISSUE-KEY --sprint SPRINT-ID

# Sprint metadata
jira sprint list --state active
jira sprint list --state future
jira sprint list --state closed
```

## Integration with Other Skills

- **TODOs**: Create sprint tasks in org-mode
- **Notes**: Sprint planning and retrospective notes
- **Calendar**: Sprint timeline and ceremonies
- **Email**: Sprint summaries to stakeholders
