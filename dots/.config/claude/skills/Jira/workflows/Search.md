# Search Jira Issues with JQL

Advanced searching using JQL (Jira Query Language) for complex filtering.

## When to Use

- User needs complex filtering beyond basic options
- Finding specific sets of issues
- Creating custom queries
- Reporting and analysis
- Cross-project searches

## Steps

1. **Understand the search criteria** from user request
2. **Construct JQL query** based on requirements
3. **Execute search** using `--jql` flag
4. **Present results** in appropriate format
5. **Offer to refine** or save the query

## JQL Basics

### Simple Query
```bash
jira issue list --jql "assignee = currentUser()"
```

### Combined Conditions (AND)
```bash
jira issue list --jql "project = SRVKP AND status = 'In Progress'"
```

### Alternative Conditions (OR)
```bash
jira issue list --jql "priority = High OR priority = Critical"
```

### Negation (NOT)
```bash
jira issue list --jql "status != Done"
```

### IN Clause
```bash
jira issue list --jql "status IN ('To Do', 'In Progress', 'Blocked')"
```

## Common JQL Fields

### User Fields
- `assignee = currentUser()` - Assigned to me
- `reporter = currentUser()` - Reported by me
- `assignee = username@redhat.com` - Specific user
- `assignee is EMPTY` - Unassigned

### Status & Priority
- `status = "In Progress"` - Specific status
- `status != Done` - Not done
- `priority IN (High, Critical)` - Multiple priorities

### Project & Type
- `project = SRVKP` - Specific project
- `type = Bug` - Issue type
- `type IN (Bug, Task)` - Multiple types

### Time-Based
- `created >= -7d` - Last 7 days
- `updated <= -30d` - Older than 30 days
- `created >= "2025-01-01"` - Specific date
- `resolved >= startOfWeek()` - This week

### Text Search
- `text ~ "affinity assistant"` - Contains text
- `summary ~ "pod"` - Summary contains
- `description ~ "workaround"` - Description contains

### Labels & Components
- `labels = documentation` - Has label
- `labels is EMPTY` - No labels
- `component = "Tekton Pipelines"` - Specific component

### Version
- `fixVersion = "1.15.0"` - Fix version
- `affectedVersion = "1.14.0"` - Affects version

### Links & Hierarchy
- `issuekey in linkedIssues(SRVKP-1234)` - Linked issues
- `parent = SRVKP-1234` - Sub-tasks of issue
- `"Epic Link" = SRVKP-1000` - Issues in epic

## JQL Functions

### User Functions
- `currentUser()` - Logged-in user
- `membersOf("team-name")` - Team members

### Time Functions
- `startOfDay()`, `endOfDay()` - Day boundaries
- `startOfWeek()`, `endOfWeek()` - Week boundaries
- `startOfMonth()`, `endOfMonth()` - Month boundaries
- `startOfYear()`, `endOfYear()` - Year boundaries

### Sprint Functions
- `sprint in openSprints()` - Current sprints
- `sprint in closedSprints()` - Past sprints
- `sprint in futureSprints()` - Upcoming sprints

### Issue Functions
- `linkedIssues(ISSUE-KEY)` - Linked issues
- `issueHistory()` - Issues you viewed

## Example Queries

### My Work
```bash
# My open issues
jira issue list --jql "assignee = currentUser() AND status != Done"

# Issues I reported
jira issue list --jql "reporter = currentUser() ORDER BY created DESC"

# Recently assigned to me
jira issue list --jql "assignee = currentUser() AND updated >= -7d"
```

### Team Queries
```bash
# Unassigned bugs
jira issue list --jql "project = SRVKP AND type = Bug AND assignee is EMPTY"

# Blocked items
jira issue list --jql "status = Blocked ORDER BY priority DESC"

# High priority open issues
jira issue list --jql "project = SRVKP AND priority IN (Critical, High) AND status != Done"
```

### Time-Based
```bash
# Stale issues (no update in 30 days)
jira issue list --jql "status != Done AND updated <= -30d ORDER BY updated ASC"

# Recent bugs
jira issue list --jql "type = Bug AND created >= -14d ORDER BY created DESC"

# This week's completed work
jira issue list --jql "assignee = currentUser() AND resolved >= startOfWeek()"
```

### Sprint & Release
```bash
# Current sprint issues
jira issue list --jql "sprint in openSprints() AND assignee = currentUser()"

# Issues missing from sprint
jira issue list --jql "project = SRVKP AND status != Done AND sprint is EMPTY"

# Release 1.15 work
jira issue list --jql "fixVersion = '1.15.0' AND status != Done"
```

### Labels & Components
```bash
# Documentation needed
jira issue list --jql "labels = docs-pending AND status = Done"

# Customer-reported bugs
jira issue list --jql "type = Bug AND labels = customer-reported"

# Tekton Pipelines component
jira issue list --jql "component = 'Tekton Pipelines' AND status != Done"
```

### Advanced
```bash
# Issues with no activity
jira issue list --jql "status IN ('To Do', 'In Progress') AND updated <= -30d AND assignee = currentUser()"

# Subtasks of epic
jira issue list --jql "parent = SRVKP-1000 ORDER BY priority DESC"

# Issues linked to specific issue
jira issue list --jql "issuekey in linkedIssues(SRVKP-1234)"

# Reopened issues
jira issue list --jql "status was Done AND status != Done"
```

## Sorting & Limiting

### ORDER BY
```bash
# By priority (descending)
jira issue list --jql "project = SRVKP ORDER BY priority DESC"

# By created date (newest first)
jira issue list --jql "assignee = currentUser() ORDER BY created DESC"

# Multiple sort fields
jira issue list --jql "status != Done ORDER BY priority DESC, updated ASC"
```

### Limit Results
```bash
# Limit in jira-cli
jira issue list --jql "project = SRVKP" --limit 20

# Limit in JQL (not supported by all Jira versions)
jira issue list --jql "project = SRVKP ORDER BY created DESC" --limit 10
```

## Saved Queries

Common queries worth saving:

### My Active Work
```jql
assignee = currentUser() AND status IN ("In Progress", "Code Review", "QE Review")
```

### Needs Attention
```jql
assignee = currentUser() AND status = Blocked
OR (assignee = currentUser() AND priority = Critical AND status != Done)
```

### Team Triage
```jql
project = SRVKP AND type = Bug AND assignee is EMPTY AND status = "To Do"
ORDER BY priority DESC, created ASC
```

### Weekly Summary
```jql
assignee = currentUser() AND updated >= startOfWeek()
ORDER BY status ASC, priority DESC
```

### Release Planning
```jql
project = SRVKP AND fixVersion is EMPTY AND status != Done
ORDER BY priority DESC
```

## Examples

### Example 1: Find Stale Work
**User**: "Show me issues I haven't updated in 2 weeks"

**Action**:
```bash
jira issue list --jql "assignee = currentUser() AND status != Done AND updated <= -14d ORDER BY updated ASC"
```

### Example 2: Sprint Planning
**User**: "What high priority issues aren't in a sprint?"

**Action**:
```bash
jira issue list --jql "project = SRVKP AND priority IN (Critical, High) AND status != Done AND sprint is EMPTY ORDER BY priority DESC"
```

### Example 3: Bug Triage
**User**: "Show unassigned customer-reported bugs"

**Action**:
```bash
jira issue list --jql "type = Bug AND labels = customer-reported AND assignee is EMPTY ORDER BY created ASC"
```

### Example 4: Weekly Review
**User**: "What did I complete this week?"

**Action**:
```bash
jira issue list --jql "assignee = currentUser() AND status changed to Done DURING (startOfWeek(), endOfWeek())"
```

## JQL Best Practices

1. **Use quotes for multi-word values**: `status = "In Progress"`
2. **Use parentheses for complex logic**: `(A OR B) AND C`
3. **Use currentUser()**: More portable than hardcoded usernames
4. **Use relative dates**: `-7d` instead of specific dates
5. **Test incrementally**: Build complex queries step by step
6. **Save common queries**: Document frequent searches
7. **Use ORDER BY**: Sort results meaningfully

## Debugging JQL

### Test in Web UI First
Use Jira's advanced search to test queries:
https://issues.redhat.com/issues/?jql=YOUR_QUERY

### Common Errors
- **Unquoted multi-word values**: Use quotes
- **Wrong field names**: Check field names in UI
- **Invalid syntax**: Check parentheses and operators
- **Permission issues**: Some fields may be restricted

### Use --debug Flag
```bash
jira issue list --jql "..." --debug
```

## Output Formatting

### Plain Text
```bash
jira issue list --jql "..." --plain
```

### Specific Columns
```bash
jira issue list --jql "..." --columns KEY,SUMMARY,STATUS,ASSIGNEE
```

### No Truncation
```bash
jira issue list --jql "..." --no-truncate
```

## Integration with Shell

### Count Results
```bash
jira issue list --jql "status = Done AND resolved >= startOfWeek()" --plain | wc -l
```

### Extract Keys
```bash
jira issue list --jql "..." --plain | awk '{print $1}'
```

### Bulk Operations
```bash
jira issue list --jql "status = 'Code Review' AND assignee = currentUser()" --plain | \
while read key rest; do
  jira issue move "$key" "QE Review"
done
```

## Tips

- **Learn gradually**: Start simple, add complexity
- **Use web UI**: Visual query builder helps
- **Document queries**: Save useful patterns
- **Combine with grep**: Filter results further
- **Use --plain**: For scripting
- **Test on small sets**: Before bulk operations
- **Check performance**: Complex queries may be slow

## Follow-up Actions

After searching:
- **Refine query**: Adjust filters
- **Save query**: Document for reuse
- **Bulk update**: Act on results
- **Export results**: Share with team
- **Create dashboard**: Monitor regularly

## Related Resources

- JQL Reference: https://issues.redhat.com/secure/JiraJQLHelp.jspa
- JQL Functions: Click "Advanced" in Jira search
- Field Names: Visible in issue view
