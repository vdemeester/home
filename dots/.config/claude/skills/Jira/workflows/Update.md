# Update Jira Issue

Update issue fields like assignee, priority, labels, or other metadata.

## When to Use

- User wants to change issue assignee
- Need to update priority or labels
- Changing issue metadata without workflow transition
- Bulk updating issue fields

## Steps

1. **Identify the issue** to update
2. **Determine which field(s)** to modify
3. **Execute update command** with new values
4. **Confirm the change**
5. **Offer additional updates** if needed

## Commands

### Assign Issue
```bash
# Assign to yourself
jira issue assign ISSUE-KEY $(jira me)

# Assign to specific user
jira issue assign ISSUE-KEY username@redhat.com

# Unassign
jira issue assign ISSUE-KEY x@
```

### Update Priority
```bash
jira issue edit ISSUE-KEY --priority Critical
```

### Add Labels
```bash
jira issue edit ISSUE-KEY --label +documentation,+release-notes
```

### Remove Labels
```bash
jira issue edit ISSUE-KEY --label -old-label
```

### Update Summary
```bash
jira issue edit ISSUE-KEY --summary "New title"
```

### Update Description
```bash
jira issue edit ISSUE-KEY --description "New description"
```

### Update Multiple Fields
```bash
jira issue edit ISSUE-KEY \
  --priority High \
  --assignee $(jira me) \
  --label +urgent
```

## Assignee Operations

### Assign to Me
```bash
jira issue assign ISSUE-KEY $(jira me)
```

### Assign to Teammate
```bash
jira issue assign ISSUE-KEY vdemeester@redhat.com
```

### Unassign (Assign to Unassigned)
```bash
jira issue assign ISSUE-KEY x@
```

### Reassign (Change Assignee)
```bash
jira issue assign ISSUE-KEY new-assignee@redhat.com
```

## Priority Updates

Available priorities:
- Blocker
- Critical
- Major
- Minor
- Trivial

```bash
# Increase priority
jira issue edit ISSUE-KEY --priority Critical

# Lower priority
jira issue edit ISSUE-KEY --priority Minor
```

## Label Management

### Add Labels (Keep Existing)
```bash
jira issue edit ISSUE-KEY --label +new-label,+another-label
```

### Remove Labels
```bash
jira issue edit ISSUE-KEY --label -old-label
```

### Replace All Labels
```bash
jira issue edit ISSUE-KEY --label bug,urgent,customer-reported
```

### Common Labels
- `+documentation` - Needs docs
- `+release-notes-pending` - Needs release notes
- `+customer-reported` - From customer
- `+upstream` - Upstream issue
- `+test-req` - Needs tests
- `+security` - Security related

## Component Updates

```bash
jira issue edit ISSUE-KEY --component "Tekton Pipelines"
```

## Fix Version / Affects Version

```bash
# Set fix version
jira issue edit ISSUE-KEY --fix-version 1.15.0

# Set affects version
jira issue edit ISSUE-KEY --affects-version 1.14.0
```

## Examples

### Example 1: Take Ownership
**User**: "Assign SRVKP-1234 to me"

**Action**:
```bash
jira issue assign SRVKP-1234 $(jira me)
```

**Response**: "Assigned SRVKP-1234 to you. Would you like to move it to In Progress?"

### Example 2: Escalate Priority
**User**: "Mark SRVKP-7327 as critical"

**Action**:
```bash
jira issue edit SRVKP-7327 --priority Critical
```

**Response**: "Updated priority to Critical. Should I add the 'urgent' label?"

### Example 3: Tag for Documentation
**User**: "Add documentation label to SRVKP-1234"

**Action**:
```bash
jira issue edit SRVKP-1234 --label +documentation,+docs-pending
```

### Example 4: Bulk Update
**User**: "Assign all high priority bugs to me"

**Action**:
```bash
# Get list of issues
jira issue list -t Bug -p High -a x@ --plain | while read key rest; do
  jira issue assign "$key" $(jira me)
done
```

## Field Reference

Common editable fields:
- `--summary`: Issue title
- `--description`: Issue description
- `--priority`: Priority level
- `--label`: Labels (use +/- for add/remove)
- `--component`: Component
- `--fix-version`: Fix version
- `--affects-version`: Affects version
- `--assignee`: Assignee (via separate command)

## Tips

- **Use $(jira me)**: Always works for current user
- **Preview before bulk**: Test on one issue first
- **Add labels incrementally**: Use +label to preserve existing
- **Document reason**: Add comment explaining major changes
- **Check permissions**: Some fields may be restricted
- **Use --web**: Open in browser for complex edits

## Bulk Operations

### Assign Multiple Issues
```bash
for issue in SRVKP-1 SRVKP-2 SRVKP-3; do
  jira issue assign "$issue" $(jira me)
done
```

### Add Label to Multiple
```bash
jira issue list --jql "project = SRVKP AND status = Done AND fixVersion = 1.15.0" --plain | \
while read key rest; do
  jira issue edit "$key" --label +released
done
```

### Update Priority Based on Criteria
```bash
# Escalate old blocked issues
jira issue list --jql "status = Blocked AND updated <= -7d" --plain | \
while read key rest; do
  jira issue edit "$key" --priority High
done
```

## Follow-up Actions

After updating:
- **Add comment**: Explain the change
- **Notify stakeholders**: If reassigning or escalating
- **Update related issues**: Sync parent/child issues
- **Check workflow**: Ensure status still makes sense
- **Document in notes**: Track important changes

## Integration

- **TODOs Skill**: Update corresponding TODOs
- **Notes Skill**: Document change reasoning
- **Email Skill**: Notify team of changes
