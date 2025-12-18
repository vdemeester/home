# Jira Skill for Claude Code

Comprehensive Jira issue management skill for Red Hat's issues.redhat.com, integrated with your Nix configuration and org-mode workflow.

## Overview

This skill provides full command-line access to Jira issues through the jira-cli tool, with automatic API token injection from passage and deep integration with your existing workflow (org-mode notes, TODOs, Git).

## Installation

Already configured! The skill is located at:
```
~/.config/claude/skills/Jira/
```

The `jira` command is wrapped with your API token automatically via Nix configuration in `/home/vincent/src/home/systems/kyushu/home.nix`.

## Structure

```
Jira/
├── skill.md              # Main skill definition
├── README.md             # This file
├── workflows/            # Workflow guides
│   ├── View.md           # View issue details
│   ├── List.md           # List and filter issues
│   ├── Create.md         # Create new issues
│   ├── Update.md         # Update issue fields
│   ├── Comment.md        # Add comments
│   ├── Search.md         # Advanced JQL searches
│   ├── LinkToNote.md     # Org-mode integration
│   └── Transition.md     # Workflow state changes
└── tools/                # Helper scripts
    ├── jira-search       # Common search patterns
    └── jira-to-org       # Convert issue to org-mode
```

## Quick Start

### View an Issue
```bash
jira issue view SRVKP-7327
```

### List Your Issues
```bash
jira issue list -a $(jira me) -s ~Done
```

### Create an Issue
```bash
jira issue create
```

### Search with JQL
```bash
jira issue list --jql "project = SRVKP AND status = 'To Do' ORDER BY priority DESC"
```

## Workflows

### 1. View Workflow
**Trigger**: User mentions issue key or asks to view issue
**Purpose**: Display detailed issue information
**Example**: "Show me SRVKP-7327"

### 2. List Workflow
**Trigger**: "list my issues", "show tickets", etc.
**Purpose**: Filter and display multiple issues
**Example**: "Show my open bugs"

### 3. Create Workflow
**Trigger**: "create issue", "file bug", "new task"
**Purpose**: Create new Jira issues
**Example**: "Create a bug for the pod creation issue"

### 4. Update Workflow
**Trigger**: "assign to", "change priority", "add label"
**Purpose**: Modify issue fields
**Example**: "Assign SRVKP-1234 to me"

### 5. Comment Workflow
**Trigger**: "add comment", "comment on"
**Purpose**: Add discussion to issues
**Example**: "Comment that I'm working on it"

### 6. Search Workflow
**Trigger**: "search for", "find issues", complex filtering
**Purpose**: Advanced JQL queries
**Example**: "Find stale high priority issues"

### 7. LinkToNote Workflow
**Trigger**: "create note for", "link to note"
**Purpose**: Integrate with org-mode denote notes
**Example**: "Create investigation note for SRVKP-7327"

### 8. Transition Workflow
**Trigger**: "start work", "mark done", "move to"
**Purpose**: Change workflow states
**Example**: "Move SRVKP-1234 to In Progress"

## Helper Tools

### jira-search
Predefined search patterns for common queries:

```bash
# My open issues
jira-search my-open

# My work in progress
jira-search my-progress

# Unassigned team bugs
jira-search team-bugs

# Stale issues
jira-search stale

# High priority items
jira-search high-priority
```

Available patterns:
- `my-open` - Your open issues
- `my-progress` - Your in-progress work
- `my-week` - Updated this week
- `my-blocked` - Your blocked issues
- `team-bugs` - Unassigned bugs
- `team-blocked` - All blocked issues
- `stale` - No updates in 30+ days
- `high-priority` - Critical/High/Blocker items
- `needs-review` - In code/QE review

### jira-to-org
Convert Jira issues to org-mode format:

```bash
# Basic conversion
jira-to-org SRVKP-1234

# Investigation template
jira-to-org SRVKP-1234 --template investigation

# Save to file
jira-to-org SRVKP-1234 --output ~/notes/issue.org
```

Templates:
- `basic` - Simple note structure
- `investigation` - Bug investigation template
- `planning` - Feature planning template
- `discussion` - Meeting/discussion notes

## Integration with Other Skills

### TODOs Skill
Create org-mode TODOs from Jira issues:
1. View issue details
2. Extract key information
3. Create TODO in todos.org
4. Link back to Jira issue

### Notes Skill
Document Jira work in denote notes:
1. Use jira-to-org to create note structure
2. Add JIRA property for linking
3. Track investigation and findings
4. Reference in Jira comments

### Git Skill
Reference issues in commits:
```bash
git commit -m "fix: Resolve pod creation issue

Fixes SRVKP-7327"
```

### Email Skill
Share issue summaries with team

## Common Use Cases

### Daily Standup Prep
```bash
# What did I do yesterday?
jira-search my-week --plain

# What am I doing today?
jira-search my-progress
```

### Bug Triage
```bash
# Unassigned bugs by priority
jira-search team-bugs --plain

# Assign to yourself
jira issue assign SRVKP-1234 $(jira me)
```

### Sprint Planning
```bash
# High priority items not in sprint
jira issue list --jql "priority IN (Critical, High) AND sprint is EMPTY"

# Add to sprint
jira sprint add SPRINT-123 SRVKP-1234
```

### Investigation Workflow
```bash
# 1. View issue
jira issue view SRVKP-7327

# 2. Create investigation note
jira-to-org SRVKP-7327 --template investigation --output ~/notes/investigation.org

# 3. Start work
jira issue assign SRVKP-7327 $(jira me)
jira issue move SRVKP-7327 "In Progress"

# 4. Add findings
# (Edit note, add findings)

# 5. Update Jira
jira issue comment add SRVKP-7327 "Root cause identified: [details]"

# 6. Complete
jira issue move SRVKP-7327 "Code Review"
```

## Configuration

### Jira Config
Located at: `~/.config/.jira/.config.yml`

Key settings:
- **server**: https://issues.redhat.com
- **installation**: local
- **auth_type**: bearer
- **project.key**: SRVKP (default)

### API Token
Managed via passage, automatically injected by Nix wrapper:
```bash
passage show redhat/issues/token/kyushu
```

### Wrapper Location
`/home/vincent/src/home/systems/kyushu/home.nix`:
```nix
jira-wrapped = pkgs.writeShellScriptBin "jira" ''
  export JIRA_API_TOKEN=$(${pkgs.passage}/bin/passage show redhat/issues/token/kyushu)
  exec ${pkgs.jira-cli-go}/bin/jira "$@"
'';
```

## Best Practices

### Always Use --plain for Processing
```bash
jira issue view ISSUE-KEY --plain
```

### Document State Changes
```bash
jira issue move ISSUE-KEY "Done"
jira issue comment add ISSUE-KEY "Completed: [details]"
```

### Link Related Work
- Reference issue keys in commits
- Link PRs in Jira comments
- Connect denote notes to issues

### Use Consistent Labels
- `documentation` - Needs docs
- `release-notes-pending` - User-facing changes
- `customer-reported` - From customers
- `test-req` - Needs tests

### Save Common Queries
Document frequently-used JQL queries in notes

## Tips & Tricks

1. **Alias common commands**:
   ```bash
   alias jira-mine='jira issue list -a $(jira me) -s ~Done'
   alias jira-todo='jira issue list -a $(jira me) -s "To Do"'
   ```

2. **Combine with shell tools**:
   ```bash
   # Count my issues
   jira-search my-open --plain | wc -l

   # Extract issue keys
   jira-search team-bugs --plain | awk '{print $1}'
   ```

3. **Batch operations**:
   ```bash
   # Add label to multiple issues
   for issue in SRVKP-{1..5}; do
     jira issue edit "$issue" --label +urgent
   done
   ```

4. **Use currentUser()**:
   Always use `$(jira me)` or `currentUser()` instead of hardcoding usernames

5. **Relative dates**:
   Use `-7d`, `startOfWeek()` instead of specific dates

## Troubleshooting

### API Token Issues
```bash
# Verify token
passage show redhat/issues/token/kyushu

# Test connection
jira me

# Check config
cat ~/.config/.jira/.config.yml
```

### Permission Errors
- Ensure you're on Red Hat VPN
- Verify project access in web UI
- Check API token scopes

### No Results
- Verify project key
- Check filter criteria
- Try with --debug flag
- Use web UI to test JQL

## Resources

- **Jira CLI Documentation**: https://github.com/ankitpokhrel/jira-cli
- **Red Hat Jira**: https://issues.redhat.com
- **JQL Reference**: https://issues.redhat.com/secure/JiraJQLHelp.jspa
- **Skill Location**: `~/.config/claude/skills/Jira/`

## Examples

### Complete Workflow Example

```bash
# Morning standup
jira-search my-progress

# Find work
jira-search team-bugs --limit 5

# Assign and start
jira issue assign SRVKP-1234 $(jira me)
jira issue move SRVKP-1234 "In Progress"

# Create investigation note
jira-to-org SRVKP-1234 --template investigation --output ~/notes/srvkp-1234.org

# Work on it...
# (Make code changes, test)

# Update Jira
jira issue comment add SRVKP-1234 "Fix implemented in PR #123"
jira issue move SRVKP-1234 "Code Review"

# End of day
jira-search my-week --plain > ~/standup-notes.txt
```

## Future Enhancements

Possible additions:
- Sprint management workflows
- Epic management workflows
- Bulk operations tools
- Dashboard generation
- Report generation
- Metrics and analytics
- Custom field support
- Attachment handling
- Time tracking workflows

## Contributing

To add new workflows:
1. Create workflow file in `workflows/`
2. Update `skill.md` workflow routing table
3. Document in this README
4. Test with real Jira issues

To add new tools:
1. Create script in `tools/`
2. Make executable: `chmod +x tools/script-name`
3. Document usage in this README
4. Add examples

## License

This skill configuration is part of your personal NixOS configuration.
