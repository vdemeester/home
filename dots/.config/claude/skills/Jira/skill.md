# Jira Issue Management

## Purpose
Interactive command-line management of Jira issues, epics, and sprints for Red Hat's issues.redhat.com. Minimizes reliance on the web interface while providing comprehensive issue tracking, workflow automation, and integration with org-mode notes.

### Context Detection

**This skill activates when:**
- User mentions "jira", "ticket", "issue", "epic", or "sprint"
- User references specific Jira issue keys (e.g., SRVKP-1234)
- Working with Red Hat Jira issues
- Need to manage issue workflows, assignments, or tracking
- User wants to automate Jira operations
- Integrating Jira issues with org-mode notes or TODOs

## Workflow Routing

When the user's request matches specific Jira operations, route to the appropriate workflow:

| Workflow | Trigger | File |
|----------|---------|------|
| **View** | "view issue", "show ticket", "get details", issue key mentioned | `workflows/View.md` |
| **List** | "list issues", "show my tickets", "what's assigned to me" | `workflows/List.md` |
| **Create** | "create issue", "new ticket", "file bug", "open task" | `workflows/Create.md` |
| **Update** | "update issue", "change status", "assign to", "move to" | `workflows/Update.md` |
| **Comment** | "add comment", "comment on", "reply to issue" | `workflows/Comment.md` |
| **Search** | "search for", "find issues", "JQL query", complex filtering | `workflows/Search.md` |
| **LinkToNote** | "link to note", "create note for issue", "document issue" | `workflows/LinkToNote.md` |
| **Sprint** | "sprint", "current sprint", "add to sprint" | `workflows/Sprint.md` |
| **Epic** | "epic", "epic issues", "add to epic" | `workflows/Epic.md` |
| **Transition** | "transition", "workflow", "move issue to", state changes | `workflows/Transition.md` |

## Key Features

### Issue Management
- View detailed issue information
- Create new issues (bugs, tasks, stories, epics)
- Update issue fields (status, assignee, priority, labels)
- Add comments and work logs
- Attach files and links

### Advanced Filtering
- List issues by assignee, reporter, project
- Filter by status, priority, type
- Time-based filtering (created, updated, resolved)
- Custom JQL queries for complex searches
- Save and reuse common filters

### Sprint & Epic Management
- List current, previous, and future sprints
- Add/remove issues from sprints
- Track epic progress
- Filter issues by epic or sprint

### Integration with Org-Mode
- Link Jira issues to org-mode notes
- Create TODOs from Jira issues
- Reference issues in project planning
- Track issue progress in daily notes

## Common Jira Projects

Red Hat Jira projects you commonly work with:
- **SRVKP**: Tekton Pipelines (Service, Kubernetes Pipelines)
- **SRVCOM**: Common services and infrastructure
- **RHCLOUD**: Red Hat Cloud services

## Best Practices

### 1. Use Plain Output for Scripting
Always use `--plain` flag when the output will be processed:
```bash
jira issue list --plain -a $(jira me) -s "To Do"
```

### 2. Use JQL for Complex Queries
For advanced filtering, use JQL (Jira Query Language):
```bash
jira issue list --jql "project = SRVKP AND assignee = currentUser() AND status != Done ORDER BY priority DESC"
```

### 3. Set Default Project
Configure your most-used project in `~/.config/.jira/.config.yml`:
```yaml
project:
  key: SRVKP
```

### 4. Integrate with Workflows
- Create org-mode TODOs for critical issues
- Link issues to notes for context
- Use jira commands in scripts and automation

### 5. Common Filters
Save time with these common queries:
- My open issues: `-a $(jira me) -s ~Done`
- High priority bugs: `-t Bug -p High,Highest`
- Recently updated: `--updated-after -7d`
- Blocked issues: `-s Blocked,Waiting`

## Configuration

### Jira Config Location
`~/.config/.jira/.config.yml`

### Key Configuration Options
- **server**: https://issues.redhat.com
- **login**: Your Red Hat email
- **project.key**: Default project (e.g., SRVKP)
- **installation**: local (for on-premise)
- **auth_type**: bearer (using API token from passage)

### API Token
Managed automatically via Nix wrapper using passage:
```bash
passage show redhat/issues/token/kyushu
```

## Jira CLI Quick Reference

### Issue Operations
```bash
# View issue
jira issue view SRVKP-1234

# Create issue
jira issue create

# List issues
jira issue list -a $(jira me)

# Update issue
jira issue move SRVKP-1234 "In Progress"

# Add comment
jira issue comment add SRVKP-1234
```

### Filtering Options
```bash
-a, --assignee    Filter by assignee
-r, --reporter    Filter by reporter
-t, --type        Filter by type (Bug, Task, Story, Epic)
-s, --status      Filter by status
-p, --priority    Filter by priority
-l, --label       Filter by labels
--created-after   Issues created after date
--updated-after   Issues updated after date
--jql            Custom JQL query
```

### Output Options
```bash
--plain          Plain text output (AI-friendly)
--no-truncate    Don't truncate long fields
--columns        Specify columns to display
--order-by       Sort order
--reverse        Reverse sort order
```

## Issue Types

Common issue types in Red Hat Jira:
- **Bug**: Software defects
- **Task**: General work items
- **Story**: User stories for features
- **Epic**: Large features or initiatives
- **Spike**: Research or investigation work
- **Sub-task**: Child issues of other issues

## Issue Priorities

- **Blocker**: Blocks development/testing
- **Critical**: System crashes, data loss
- **Major**: Major functionality broken
- **Minor**: Minor functionality issue
- **Trivial**: Cosmetic issues

## Workflow States

Common workflow transitions:
- **To Do** → **In Progress**: Start work
- **In Progress** → **Code Review**: Submit for review
- **Code Review** → **QE Review**: Pass to QA
- **QE Review** → **Done**: Complete
- **Any** → **Blocked**: Mark as blocked

## Integration Patterns

### 1. Daily Standup Prep
```bash
# Get issues I worked on yesterday
jira issue list --plain -a $(jira me) --updated-after -1d

# Get issues I'm working on today
jira issue list --plain -a $(jira me) -s "In Progress"
```

### 2. Create TODO from Issue
```bash
# View issue and create corresponding TODO
jira issue view SRVKP-1234 --plain
# Then use TODOs skill to add to org-mode
```

### 3. Link to Org Notes
```bash
# View issue and create denote note with issue link
jira issue view SRVKP-1234
# Create note using Notes skill with Jira URL
```

### 4. Sprint Planning
```bash
# List current sprint issues
jira sprint list --current

# Add issue to sprint
jira sprint add SPRINT-123 SRVKP-1234
```

## Tips

1. **Use aliases for common commands**: Add shell aliases for frequent operations
2. **Combine with grep/jq**: Filter jira output for specific data
3. **Use --plain in scripts**: Ensures consistent, parseable output
4. **Save JQL queries**: Keep complex queries in notes for reuse
5. **Set project defaults**: Configure common projects to reduce typing
6. **Use issue keys in commit messages**: Link commits to issues
7. **Comment from CLI**: Add quick updates without opening browser
8. **Track time efficiently**: Log work time from command line

## Troubleshooting

### Authentication Issues
- Verify API token in passage: `passage show redhat/issues/token/kyushu`
- Check config: `cat ~/.config/.jira/.config.yml`
- Test connection: `jira me`

### Permission Errors
- Verify you're on Red Hat VPN if required
- Check project permissions in web UI
- Ensure API token has correct scopes

### No Issues Found
- Verify project key is correct
- Check filter criteria (status, assignee, etc.)
- Try with `--jql` for direct query
- Use `--debug` flag to see API calls

## Related Skills

- **TODOs**: Create TODOs from Jira issues
- **Notes**: Document issues in denote notes
- **Git**: Reference issues in commits
- **Email**: Email issue summaries to team

## Learn More

- Jira CLI GitHub: https://github.com/ankitpokhrel/jira-cli
- Red Hat Jira: https://issues.redhat.com
- JQL Reference: https://issues.redhat.com/secure/JiraJQLHelp.jspa
