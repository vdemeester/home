# Transition Jira Issue Through Workflow

Move issues through workflow states (To Do → In Progress → Done, etc.).

## When to Use

- User wants to change issue status
- Moving issue to next workflow step
- Starting or completing work
- Marking issues as blocked or resolved

## Steps

1. **Identify the issue** to transition
2. **Determine target state** (e.g., In Progress, Done)
3. **Check available transitions** if unsure
4. **Execute transition** command
5. **Confirm success** and offer follow-up

## Commands

### Move to State
```bash
jira issue move ISSUE-KEY "In Progress"
```

### List Available Transitions
```bash
jira issue transitions ISSUE-KEY
```

### Interactive Transition
```bash
jira issue move ISSUE-KEY
```
Shows menu of available transitions

## Common Workflow States

Red Hat Jira typical workflow:
- **To Do** - Not started
- **In Progress** - Actively working
- **Code Review** - Implementation done, awaiting review
- **QE Review** - Code reviewed, awaiting QA
- **Blocked** - Waiting on external dependency
- **Done** - Completed and verified

## Common Transitions

### Start Work
```bash
jira issue move ISSUE-KEY "In Progress"
```

### Submit for Review
```bash
jira issue move ISSUE-KEY "Code Review"
```

### Pass to QA
```bash
jira issue move ISSUE-KEY "QE Review"
```

### Mark Complete
```bash
jira issue move ISSUE-KEY "Done"
```

### Mark Blocked
```bash
jira issue move ISSUE-KEY "Blocked"
```

### Reopen
```bash
jira issue move ISSUE-KEY "To Do"
```

## Workflow Patterns

### Standard Flow
```
To Do → In Progress → Code Review → QE Review → Done
```

### With Blocking
```
Any State → Blocked → [Previous State]
```

### Reopening
```
Done → To Do (or In Progress)
```

## Examples

### Example 1: Start Working
**User**: "Start work on SRVKP-1234"

**Actions**:
1. Transition to In Progress:
```bash
jira issue move SRVKP-1234 "In Progress"
```

2. Optionally assign to yourself:
```bash
jira issue assign SRVKP-1234 $(jira me)
```

3. Optionally add comment:
```bash
jira issue comment add SRVKP-1234 "Starting work on this issue"
```

**Response**: "Moved SRVKP-1234 to In Progress and assigned to you."

### Example 2: Submit for Review
**User**: "SRVKP-7327 is ready for review"

**Actions**:
```bash
jira issue move SRVKP-7327 "Code Review"
jira issue comment add SRVKP-7327 "Implementation complete. PR: https://github.com/org/repo/pull/123"
```

### Example 3: Mark as Blocked
**User**: "SRVKP-5678 is blocked by another issue"

**Actions**:
```bash
jira issue move SRVKP-5678 "Blocked"
jira issue comment add SRVKP-5678 "Blocked by SRVKP-9999. Cannot proceed until that's resolved."
```

### Example 4: Complete Work
**User**: "Mark SRVKP-1234 as done"

**Actions**:
```bash
jira issue move SRVKP-1234 "Done"
jira issue comment add SRVKP-1234 "Completed. Tested and verified in production."
```

### Example 5: Check Available Transitions
**User**: "What can I do with SRVKP-1234?"

**Action**:
```bash
jira issue transitions SRVKP-1234
```

Shows available transitions for the issue's current state.

## Bulk Transitions

### Move Multiple Issues
```bash
for issue in SRVKP-1 SRVKP-2 SRVKP-3; do
  jira issue move "$issue" "In Progress"
done
```

### Move Based on Criteria
```bash
# Start all my To Do tasks
jira issue list --jql "assignee = currentUser() AND status = 'To Do'" --plain | \
while read key rest; do
  jira issue move "$key" "In Progress"
  jira issue comment add "$key" "Starting work"
done
```

### Complete Sprint Items
```bash
# Mark current sprint tasks as done
jira issue list --jql "sprint in openSprints() AND assignee = currentUser() AND status = 'QE Review'" --plain | \
while read key rest; do
  jira issue move "$key" "Done"
done
```

## Workflow Best Practices

### 1. Assign Before Starting
```bash
jira issue assign ISSUE-KEY $(jira me)
jira issue move ISSUE-KEY "In Progress"
```

### 2. Add Comments on Transition
```bash
jira issue move ISSUE-KEY "Code Review"
jira issue comment add ISSUE-KEY "Ready for review. PR: [link]"
```

### 3. Document Blocks
```bash
jira issue move ISSUE-KEY "Blocked"
jira issue comment add ISSUE-KEY "Blocked because: [reason]. Blocking issue: [ISSUE-KEY]"
```

### 4. Log Work on Completion
```bash
jira issue move ISSUE-KEY "Done"
jira worklog add ISSUE-KEY "4h" "Implemented feature X and tests"
```

### 5. Update Related Issues
```bash
# Complete task and update epic
jira issue move SRVKP-1234 "Done"
jira issue comment add SRVKP-1000 "Subtask SRVKP-1234 completed"
```

## Common Transition Scenarios

### Scenario 1: Taking Over Work
```bash
# Assign and start
jira issue assign ISSUE-KEY $(jira me)
jira issue move ISSUE-KEY "In Progress"
jira issue comment add ISSUE-KEY "Taking over this work"
```

### Scenario 2: Handing Off
```bash
# Move to review and notify
jira issue move ISSUE-KEY "Code Review"
jira issue comment add ISSUE-KEY "[~reviewer] Ready for your review"
```

### Scenario 3: Unblocking
```bash
# Move from blocked back to in progress
jira issue move ISSUE-KEY "In Progress"
jira issue comment add ISSUE-KEY "Blocker resolved, resuming work"
```

### Scenario 4: Closing Duplicate
```bash
# Link to original and close
jira issue link ISSUE-KEY ORIGINAL-KEY "duplicates"
jira issue move ISSUE-KEY "Done"
jira issue comment add ISSUE-KEY "Duplicate of [ORIGINAL-KEY]"
```

## Validation Checks

Before transitioning, consider:
- **Assignee**: Is it assigned to the right person?
- **Sprint**: Is it in a sprint if needed?
- **Linked issues**: Are dependencies resolved?
- **Required fields**: Are all mandatory fields filled?
- **Acceptance criteria**: Are they met (for Done)?

## State-Specific Actions

### When Moving to "In Progress"
- Assign to yourself
- Add comment about approach
- Update story points if needed
- Link to related work

### When Moving to "Code Review"
- Add PR link in comment
- Tag reviewers
- Ensure tests pass
- Update documentation

### When Moving to "Done"
- Verify acceptance criteria
- Log work time
- Update fix version
- Add release notes if needed

### When Moving to "Blocked"
- Document blocker clearly
- Link blocking issue
- Set target resolution date
- Notify stakeholders

## Follow-up Actions

After transition:
- **Update TODO**: Sync with org-mode TODOs
- **Notify team**: If affects others
- **Update notes**: Document in denote notes
- **Check dependencies**: Update linked issues
- **Plan next**: Identify next task

## Tips

- **Use autocomplete**: Type first letters of state
- **Check transitions**: Use `transitions` command if unsure
- **Add context**: Always comment on major transitions
- **Be consistent**: Follow team conventions
- **Update promptly**: Don't let states go stale
- **Consider dependencies**: Check linked issues
- **Track time**: Log work on meaningful transitions

## Integration

- **TODOs Skill**: Update org-mode TODO states
- **Notes Skill**: Document transition reasoning
- **Git Skill**: Reference in commits
- **Email Skill**: Notify stakeholders
