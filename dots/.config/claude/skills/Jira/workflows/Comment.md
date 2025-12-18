# Add Comment to Jira Issue

Add comments, updates, or discussion to Jira issues.

## When to Use

- User wants to add a comment to an issue
- Need to provide update or status
- Responding to question in issue
- Documenting progress or findings
- Communicating with team on issue

## Steps

1. **Identify the issue** to comment on
2. **Prepare the comment** text
3. **Add the comment** using jira command
4. **Confirm addition**
5. **Offer to view** the updated issue

## Commands

### Interactive Comment
```bash
jira issue comment add ISSUE-KEY
```
Opens editor for multi-line comment

### Inline Comment
```bash
jira issue comment add ISSUE-KEY "Comment text here"
```

### Comment from File
```bash
jira issue comment add ISSUE-KEY --from-file comment.txt
```

### View Comments
```bash
jira issue view ISSUE-KEY --comments 10
```

### Comment with Mention
```bash
jira issue comment add ISSUE-KEY "[~vdemeester] Can you take a look at this?"
```

## Comment Formatting

### Mention User
```
[~username] or [@username]
```

### Link to Issue
```
SRVKP-1234 (auto-linked)
```

### Code Block
```
{code:bash}
kubectl get pods
{code}
```

### Quote
```
{quote}
Original text here
{quote}
```

### Bold/Italic
```
*bold text*
_italic text_
```

### Lists
```
* Item 1
* Item 2
** Sub-item 2a
```

### Links
```
[Link text|https://example.com]
```

## Examples

### Example 1: Quick Update
**User**: "Comment on SRVKP-7327 that I'm investigating this"

**Action**:
```bash
jira issue comment add SRVKP-7327 "Investigating this issue. Will provide update by EOD."
```

**Response**: "Added comment. Would you like to change the status to In Progress?"

### Example 2: Provide Solution
**User**: "Add comment with the workaround for SRVKP-1234"

**Action**:
```bash
jira issue comment add SRVKP-1234 "$(cat <<'EOF'
*Workaround Found*

Set the following SCC to the default service account:

{code:bash}
oc adm policy add-scc-to-user anyuid system:serviceaccount:default:default
{code}

This allows the affinity assistant pod to be created successfully.
EOF
)"
```

### Example 3: Tag Team Member
**User**: "Ask Vincent to review SRVKP-5678"

**Action**:
```bash
jira issue comment add SRVKP-5678 "[~vdemeester] Could you review this when you have time? Thanks!"
```

### Example 4: Progress Update
**User**: "Comment that I've finished the implementation"

**Action**:
```bash
jira issue comment add ISSUE-KEY "Implementation completed. Ready for code review.

Changes:
* Added feature X
* Updated tests
* Updated documentation

PR: https://github.com/org/repo/pull/123"
```

## Common Comment Patterns

### Status Update
```bash
jira issue comment add ISSUE-KEY "Working on this. Currently investigating the root cause."
```

### Blocker Notification
```bash
jira issue comment add ISSUE-KEY "Blocked by SRVKP-9999. Cannot proceed until that's resolved."
```

### Request for Information
```bash
jira issue comment add ISSUE-KEY "[~reporter] Can you provide more details about how to reproduce this?"
```

### Resolution Summary
```bash
jira issue comment add ISSUE-KEY "Fixed in PR #123. The issue was caused by X. Solution implemented: Y."
```

### Testing Results
```bash
jira issue comment add ISSUE-KEY "Tested on environment X. All scenarios passed. Ready for QE review."
```

## Multi-line Comments

For longer comments, use heredoc:

```bash
jira issue comment add ISSUE-KEY "$(cat <<'EOF'
# Investigation Summary

## Root Cause
The issue occurs because...

## Proposed Fix
We should...

## Testing Plan
1. Test scenario A
2. Test scenario B
3. Verify no regression

## Timeline
- Implementation: 2 days
- Testing: 1 day
- Review: 1 day
EOF
)"
```

## Comment Best Practices

### 1. Be Clear and Concise
- State the purpose upfront
- Use formatting for readability
- Break up long text into sections

### 2. Provide Context
- Reference related issues
- Link to PRs, commits, docs
- Include relevant code snippets

### 3. Tag Appropriately
- Mention users who need to see it
- Use @username or [~username]
- Don't over-notify

### 4. Use Formatting
- Code blocks for code
- Lists for steps or items
- Bold for important points
- Links for references

### 5. Keep Professional
- Focus on facts and solutions
- Be constructive in feedback
- Use neutral tone

## Viewing Comments

### View All Comments
```bash
jira issue view ISSUE-KEY --comments 100
```

### View Recent Comments
```bash
jira issue view ISSUE-KEY --comments 5
```

### Plain Text Comments
```bash
jira issue view ISSUE-KEY --comments 10 --plain
```

## Comment Templates

### Investigation Update
```
*Investigation Update*

Status: In Progress
Findings:
* Finding 1
* Finding 2

Next Steps:
* Step 1
* Step 2

ETA: [date]
```

### Fix Applied
```
*Fix Applied*

Changes: [PR link or commit]
Testing: Passed local tests
Deployment: Ready for QE

Resolves: [describe how it fixes the issue]
```

### Needs Information
```
*Additional Information Needed*

To proceed, we need:
1. [Item 1]
2. [Item 2]

[~reporter] Could you provide these details?
```

### Blocked
```
*Issue Blocked*

Blocked by: SRVKP-XXXX
Reason: [explain dependency]
Workaround: [if available]

Can we prioritize the blocker?
```

## Follow-up Actions

After commenting:
- **Change status**: If comment indicates progress
- **Update labels**: Add relevant tags
- **Notify team**: Send email if urgent
- **Link issues**: Connect related work
- **Log work**: Track time spent

## Tips

- **Use templates**: Save common comment patterns
- **Format code**: Always use code blocks
- **Link everything**: PRs, commits, docs
- **Be timely**: Comment when you make progress
- **Document decisions**: Explain why, not just what
- **Use mentions sparingly**: Notify only relevant people
- **Edit if needed**: Fix mistakes or add clarification

## Integration

- **Notes Skill**: Copy comment content to notes
- **Email Skill**: Send comment summary to stakeholders
- **Git**: Reference issue in commits with similar message
