# List Jira Issues

List and filter Jira issues based on various criteria.

## When to Use

- User asks to see their issues/tickets
- User wants to filter issues by status, priority, assignee, etc.
- Need to get an overview of work items
- Preparing for standup or sprint planning
- Finding issues to work on

## Steps

1. **Determine filter criteria** from user request
2. **Construct appropriate jira command** with filters
3. **Execute and present results** in a clear format
4. **Offer refinement or actions** on the listed issues

## Common Commands

### My Open Issues
```bash
jira issue list -a $(jira me) -s ~Done
```

### My Current Work
```bash
jira issue list -a $(jira me) -s "In Progress"
```

### High Priority Issues
```bash
jira issue list -p Critical,Blocker,High -s ~Done
```

### Recent Bugs
```bash
jira issue list -t Bug --created-after -7d
```

### Unassigned Issues in Project
```bash
jira issue list -a x@
```

### Issues Updated Recently
```bash
jira issue list --updated-after -3d
```

### Plain Text List (for processing)
```bash
jira issue list --plain -a $(jira me)
```

## Filter Options

### By Assignee
- `-a USERNAME` or `-a $(jira me)` for yourself
- `-a x@` for unassigned issues

### By Status
- `-s "To Do"` - Specific status
- `-s "To Do","In Progress"` - Multiple statuses
- `-s ~Done` - NOT Done (all open)

### By Type
- `-t Bug` - Bugs only
- `-t Task,Story` - Multiple types
- `-t Epic` - Epics only

### By Priority
- `-p Critical,Blocker` - High priority
- `-p Minor,Trivial` - Low priority

### By Time
- `--created-after -7d` - Created in last 7 days
- `--updated-after -1w` - Updated in last week
- `--created-before 2025-01-01` - Before specific date

### By Labels
- `-l documentation,bug` - Has specific labels
- `-l release-notes-pending` - Needs release notes

### Custom Columns
```bash
jira issue list --columns TYPE,KEY,SUMMARY,STATUS,ASSIGNEE
```

### Limit Results
```bash
jira issue list -a $(jira me) --limit 20
```

## Output Formatting

### Default View
Shows table with key, summary, status, assignee

### Plain Text (--plain)
Best for scripting and AI processing:
```bash
jira issue list --plain -a $(jira me) -s "In Progress"
```

### No Truncation (--no-truncate)
Shows full field values:
```bash
jira issue list --no-truncate
```

### Reverse Order (--reverse)
Reverse the sort order:
```bash
jira issue list --reverse
```

## Examples

### Example 1: Daily Standup Prep
**User**: "What am I working on?"

**Action**:
```bash
jira issue list -a $(jira me) -s "In Progress","Code Review"
```

**Presentation**: List issues with status, highlighting blockers

### Example 2: Find Something to Work On
**User**: "Show me unassigned tasks in SRVKP"

**Action**:
```bash
jira issue list -a x@ -t Task -s "To Do" --limit 10
```

**Follow-up**: "Would you like me to assign any of these to you?"

### Example 3: Check Team Progress
**User**: "What bugs are still open?"

**Action**:
```bash
jira issue list -t Bug -s ~Done --order-by priority
```

**Presentation**: Group by priority, show counts

### Example 4: Sprint Planning
**User**: "Show high priority items not in a sprint"

**Action**:
```bash
jira issue list -p High,Critical --jql "sprint is EMPTY"
```

## Advanced JQL Queries

For complex queries, use `--jql`:

### Issues I Reported
```bash
jira issue list --jql "reporter = currentUser()"
```

### Stale Issues
```bash
jira issue list --jql "status != Done AND updated <= -30d"
```

### Issues Needing Review
```bash
jira issue list --jql "status = 'Code Review' AND assignee = currentUser()"
```

### Blocked Issues
```bash
jira issue list --jql "status = Blocked ORDER BY priority DESC"
```

## Common Patterns

### 1. Daily Standup
```bash
# What I did yesterday
jira issue list -a $(jira me) --updated-after -1d --plain

# What I'm doing today
jira issue list -a $(jira me) -s "In Progress" --plain
```

### 2. Sprint Overview
```bash
# All sprint issues by status
jira issue list --jql "sprint in openSprints() GROUP BY status"
```

### 3. Bug Triage
```bash
# Unassigned bugs by priority
jira issue list -t Bug -a x@ --order-by priority --reverse
```

### 4. Release Planning
```bash
# Issues with release label
jira issue list -l release-2.0 -s ~Done
```

## Follow-up Actions

After listing issues, offer to:
- **View specific issue**: Get details on interesting item
- **Update filters**: Refine the search
- **Assign issues**: Take ownership
- **Create TODO**: Add to personal task list
- **Export list**: Save to file or share

## Tips

- **Use --plain for scripting**: Easier to parse
- **Combine filters**: Narrow down results effectively
- **Save common queries**: Document frequent JQL patterns
- **Use $(jira me)**: Always works regardless of username
- **Check counts first**: Use `--limit` for large result sets
- **Group by fields**: Use JQL GROUP BY for summaries
- **Order by priority**: Show most important first

## Integration

- **TODOs Skill**: Convert list to org-mode TODOs
- **Notes Skill**: Document planning in notes
- **Email Skill**: Send list summaries to team
