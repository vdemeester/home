# Create Jira Issue

Create new Jira issues (bugs, tasks, stories, epics).

## When to Use

- User wants to create a new ticket
- User says "file a bug", "create task", "new issue"
- Need to track work or report a problem
- Converting discussion or notes into actionable items

## Steps

1. **Determine issue type** (Bug, Task, Story, Epic)
2. **Gather required information**:
   - Summary (title)
   - Description
   - Project key (default: SRVKP)
   - Priority (optional)
   - Labels (optional)
   - Assignee (optional)
3. **Create issue** using interactive or non-interactive mode
4. **Confirm creation** and provide issue key
5. **Offer follow-up actions** (assign, link to note, add to sprint)

## Commands

### Interactive Create
```bash
jira issue create
```
Prompts for all fields interactively

### Create with Template
```bash
jira issue create -t
```
Uses project template

### Create Bug
```bash
jira issue create \
  --type Bug \
  --summary "Affinity assistant pod not created" \
  --priority Major \
  --label bug,scc
```

### Create Task
```bash
jira issue create \
  --type Task \
  --summary "Update documentation for feature X" \
  --assignee $(jira me)
```

### Create from File
```bash
jira issue create --template issue-template.json
```

## Required Fields

### All Issues
- **Project**: Usually SRVKP (can be set as default)
- **Issue Type**: Bug, Task, Story, Epic, Spike, Sub-task
- **Summary**: Brief title (should be descriptive)

### Bugs
- **Description**: Steps to reproduce, actual/expected behavior
- **Priority**: Severity of the issue
- **Affects Version**: Which version has the bug
- **Environment**: OS, platform, configuration

### Stories/Tasks
- **Description**: What needs to be done and why
- **Acceptance Criteria**: Definition of done

### Epics
- **Epic Name**: Short name for the epic
- **Description**: High-level goal

## Issue Types

### Bug
Software defects that need fixing
```bash
jira issue create \
  --type Bug \
  --summary "Title" \
  --priority Major \
  --description "$(cat <<'EOF'
Description of problem:
Steps to reproduce:
1.
2.
3.

Actual results:


Expected results:


Reproducibility: Always/Intermittent/Only Once
EOF
)"
```

### Task
General work items
```bash
jira issue create \
  --type Task \
  --summary "Implement feature X" \
  --description "Technical details..."
```

### Story
User-focused features
```bash
jira issue create \
  --type Story \
  --summary "As a user, I want to..." \
  --description "User story and acceptance criteria"
```

### Epic
Large initiatives
```bash
jira issue create \
  --type Epic \
  --summary "Epic: Major feature set" \
  --description "High-level goals and sub-initiatives"
```

## Priority Levels

- **Blocker**: Blocks development/testing, needs immediate attention
- **Critical**: System crashes, data loss, no workaround
- **Major**: Major functionality broken, workaround exists
- **Minor**: Minor functionality issue
- **Trivial**: Cosmetic issues, nice-to-have

## Common Labels

Red Hat Jira common labels:
- `bug` - Bug fix
- `feature` - New feature
- `documentation` - Docs work
- `test` - Testing work
- `release-notes-pending` - Needs release notes
- `docs-pending` - Needs documentation
- `customer-reported` - From customer
- `security` - Security issue

## Examples

### Example 1: Quick Bug Report
**User**: "File a bug that the affinity assistant pod isn't being created"

**Action**:
```bash
jira issue create \
  --type Bug \
  --summary "Affinity assistant pod not created" \
  --priority Major \
  --label bug,docs-pending \
  --description "Description of the issue..."
```

**Response**: "Created SRVKP-7327. Would you like me to assign it to you or add it to the current sprint?"

### Example 2: Task from Discussion
**User**: "Create a task to update the beets configuration documentation"

**Action**:
```bash
jira issue create \
  --type Task \
  --summary "Update beets configuration documentation" \
  --assignee $(jira me) \
  --label documentation
```

### Example 3: Interactive Creation
**User**: "Create a new issue"

**Action**: Run `jira issue create` and guide user through prompts:
1. Project: SRVKP (or ask)
2. Issue Type: (ask user)
3. Summary: (ask user)
4. Description: (ask user to provide details)
5. Additional fields as needed

## Best Practices

### 1. Clear Summaries
- Be specific and descriptive
- Include key information in title
- Avoid vague titles like "Fix bug" or "Update code"

**Good**: "Affinity assistant pod fails to create with default serviceAccount"
**Bad**: "Pod issue"

### 2. Detailed Descriptions
For bugs:
- Steps to reproduce
- Actual vs expected behavior
- Environment details
- Reproducibility

For tasks/stories:
- Context and motivation
- Acceptance criteria
- Technical notes if relevant

### 3. Appropriate Priority
- Don't over-prioritize everything as Critical
- Consider actual impact on users/system
- Align with team conventions

### 4. Useful Labels
- Add relevant labels for filtering
- Include `release-notes-pending` if user-facing
- Add `docs-pending` if docs needed
- Tag with component or area

### 5. Link Related Issues
- Reference related issues in description
- Use "relates to", "blocks", "is blocked by"
- Link to epics or parent issues

## Description Templates

### Bug Template
```markdown
### Description of problem:
[What's wrong]

### Prerequisites:
[Setup, operators/versions]

### Steps to Reproduce:
1. [First step]
2. [Second step]
3. [Third step]

### Actual results:
[What happens]

### Expected results:
[What should happen]

### Reproducibility:
Always / Intermittent / Only Once

### Additional info:
[Logs, screenshots, etc]
```

### Task Template
```markdown
### Objective:
[What needs to be done]

### Context:
[Why this is needed]

### Acceptance Criteria:
- [ ] [Criterion 1]
- [ ] [Criterion 2]
- [ ] [Criterion 3]

### Technical Notes:
[Implementation details]
```

## Follow-up Actions

After creating an issue:
- **Assign it**: To yourself or team member
- **Add to sprint**: Include in current sprint
- **Link to epic**: Associate with larger initiative
- **Create note**: Document in org-mode
- **Add TODO**: Track in personal task list
- **Share**: Send link to team

## Non-Interactive Examples

### Create and Assign
```bash
jira issue create \
  --type Task \
  --summary "Title" \
  --assignee $(jira me) \
  --no-input
```

### Create with Labels
```bash
jira issue create \
  --type Bug \
  --summary "Title" \
  --label bug,critical,customer-reported \
  --priority Critical
```

### Create Sub-task
```bash
jira issue create \
  --type Sub-task \
  --summary "Subtask title" \
  --parent SRVKP-1234
```

## Tips

- **Use templates**: Save common issue patterns
- **Set defaults**: Configure default project/type in config
- **Copy from similar**: Base on previous issues
- **Include context**: Link to docs, PRs, commits
- **Tag appropriately**: Make issues discoverable
- **Assign on creation**: If you know the owner
- **Link immediately**: Connect to epics/sprints early

## Integration

- **Notes Skill**: Reference issue in denote notes
- **TODOs Skill**: Create corresponding TODO
- **Git**: Reference issue key in commits
- **Email**: Notify team about new issue
