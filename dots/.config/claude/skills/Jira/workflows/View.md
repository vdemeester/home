# View Jira Issue

View detailed information about a Jira issue.

## When to Use

- User asks to view/show/display a specific issue
- User mentions an issue key (e.g., SRVKP-1234)
- Need to see issue details, comments, or status
- Want to understand issue context before taking action

## Steps

1. **Identify the issue key** from the user's request or ask if not provided
2. **Fetch issue details** using `jira issue view`
3. **Present relevant information** clearly
4. **Offer follow-up actions** based on context

## Commands

### Basic View
```bash
jira issue view ISSUE-KEY
```

### View with Comments
```bash
jira issue view ISSUE-KEY --comments 10
```

### Plain Text View (for processing)
```bash
jira issue view ISSUE-KEY --plain
```

### View with Full Details
```bash
jira issue view ISSUE-KEY --no-truncate
```

## Information Displayed

Standard issue view includes:
- **Issue key and summary**: The identifier and title
- **Type and priority**: Bug/Task/Story and importance level
- **Status**: Current workflow state
- **Assignee**: Who's working on it
- **Reporter**: Who created it
- **Created/Updated dates**: Timeline information
- **Description**: Full issue description
- **Labels and components**: Categorization
- **Comments**: Recent discussion (use --comments for more)
- **Links**: Related issues, epics, sprints

## Follow-up Actions

After viewing an issue, ask the user if they want to:
- **Update status**: Move to different workflow state
- **Add comment**: Contribute to discussion
- **Assign**: Change assignee
- **Create note**: Link to org-mode note for documentation
- **Create TODO**: Add to personal task list
- **View related issues**: Check linked issues or epic

## Examples

### Example 1: View Specific Issue
**User**: "Show me SRVKP-7327"

**Action**:
```bash
jira issue view SRVKP-7327
```

**Follow-up**: "Would you like to add a comment, update the status, or create a note about this issue?"

### Example 2: View with Context
**User**: "What's the status of the affinity assistant bug?"

**Action**:
1. Search for issue if key not known: `jira issue list --jql "text ~ 'affinity assistant' AND type = Bug"`
2. View the issue: `jira issue view SRVKP-7327`
3. Highlight the current status and key information

### Example 3: Quick Summary
**User**: "Give me a quick summary of SRVKP-1234"

**Action**:
```bash
jira issue view SRVKP-1234 --plain
```
Then extract and present:
- Current status
- Assignee
- Priority
- Brief description
- Last update date

## Tips

- **Use --plain** when you need to extract specific fields
- **Check comments** for recent updates and discussion
- **Look at labels** to understand categorization
- **Note the assignee** to know who to contact
- **Check linked issues** for related work
- **Review sprint/epic** to understand broader context

## Integration with Other Skills

- **Notes**: Create a denote note with `[[https://issues.redhat.com/browse/ISSUE-KEY][ISSUE-KEY]]` link
- **TODOs**: Add a TODO if action is needed
- **Email**: Draft email to assignee if needed
