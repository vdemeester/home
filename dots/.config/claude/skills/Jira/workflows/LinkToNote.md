# Link Jira Issue to Org-Mode Note

Create or link Jira issues with org-mode denote notes for documentation and context.

## When to Use

- User wants to document a Jira issue in detail
- Need to track investigation or research
- Creating comprehensive issue analysis
- Linking work context with issues
- Project planning with Jira integration

## Steps

1. **Identify the Jira issue** to document
2. **Fetch issue details** for context
3. **Create denote note** with appropriate tags
4. **Add Jira issue link** to note
5. **Optionally add note link** to Jira comment
6. **Confirm creation** and offer next actions

## Integration Pattern

### 1. Fetch Issue Information
```bash
jira issue view ISSUE-KEY --plain
```

### 2. Create Denote Note
Use Notes skill to create note with:
- **Title**: "ISSUE-KEY: Issue Summary"
- **Tags**: `jira`, project tag (e.g., `tekton`), issue type (e.g., `bug`)
- **Keywords**: Relevant keywords from issue

### 3. Add Jira Link to Note
```org
* ISSUE-KEY: Issue Summary

:PROPERTIES:
:JIRA: https://issues.redhat.com/browse/ISSUE-KEY
:CREATED: [timestamp]
:END:

[[https://issues.redhat.com/browse/ISSUE-KEY][View in Jira]]

** Issue Details
- Status: [status]
- Priority: [priority]
- Assignee: [assignee]
- Type: [type]

** Description
[Issue description]

** Investigation Notes
[Your notes here]

** Related Work
- Link to related notes
- Link to code changes
- Link to documentation
```

### 4. Link Note to Jira (Optional)
```bash
jira issue comment add ISSUE-KEY "Documentation: file://path/to/note.org"
```

## Examples

### Example 1: Document Bug Investigation
**User**: "Create a note for investigating SRVKP-7327"

**Actions**:
1. Fetch issue:
```bash
jira issue view SRVKP-7327 --plain
```

2. Create note with Notes skill:
- Title: "SRVKP-7327: Affinity assistant pod not created"
- Tags: `jira`, `tekton`, `bug`, `affinity-assistant`

3. Populate note with issue details and add sections for:
- Root cause analysis
- Investigation steps
- Testing notes
- Solution approach

4. Add comment to Jira:
```bash
jira issue comment add SRVKP-7327 "Investigation notes: [link to note]"
```

### Example 2: Feature Planning
**User**: "Create a planning note for the new epic SRVKP-8000"

**Actions**:
1. Fetch epic details
2. Create note with:
   - Epic summary
   - Goals and requirements
   - Sub-tasks breakdown
   - Technical approach
3. Link sub-tasks as they're created

### Example 3: Link Existing Note
**User**: "Link my note about affinity assistants to SRVKP-7327"

**Actions**:
1. Find the note using Notes skill
2. Add Jira property to note:
```org
:PROPERTIES:
:JIRA: https://issues.redhat.com/browse/SRVKP-7327
:END:
```
3. Add comment to Jira with note reference

## Note Templates

### Bug Investigation Note
```org
* SRVKP-XXXX: Bug Summary

:PROPERTIES:
:JIRA: https://issues.redhat.com/browse/SRVKP-XXXX
:CREATED: [timestamp]
:END:

[[https://issues.redhat.com/browse/SRVKP-XXXX][View in Jira]]

** Issue Details
- Status: To Do
- Priority: Major
- Assignee: username
- Reporter: reporter-name
- Type: Bug

** Description
[Issue description from Jira]

** Reproduction Steps
1. Step 1
2. Step 2
3. Step 3

** Investigation
*** Hypothesis
[Initial thoughts on root cause]

*** Findings
- Finding 1
- Finding 2

*** Root Cause
[Identified root cause]

** Solution
*** Proposed Fix
[Description of fix]

*** Testing Plan
- [ ] Test case 1
- [ ] Test case 2
- [ ] Regression testing

*** Implementation Notes
[Technical details]

** Related
- Related issues: [[SRVKP-YYYY]]
- PRs: [[link]]
- Documentation: [[link]]
```

### Feature Planning Note
```org
* SRVKP-XXXX: Feature Name

:PROPERTIES:
:JIRA: https://issues.redhat.com/browse/SRVKP-XXXX
:TYPE: Epic
:CREATED: [timestamp]
:END:

** Overview
[Feature description]

** Goals
- Goal 1
- Goal 2
- Goal 3

** Requirements
*** Functional Requirements
- Requirement 1
- Requirement 2

*** Non-Functional Requirements
- Performance
- Security
- Scalability

** Design
*** Architecture
[Architecture overview]

*** Components
- Component 1: [description]
- Component 2: [description]

*** Data Model
[Data structures]

** Implementation Plan
*** Phase 1: [Name]
- [ ] Task 1 - SRVKP-XXX1
- [ ] Task 2 - SRVKP-XXX2

*** Phase 2: [Name]
- [ ] Task 3 - SRVKP-XXX3
- [ ] Task 4 - SRVKP-XXX4

** Testing Strategy
- Unit tests
- Integration tests
- E2E tests

** Documentation
- User documentation
- API documentation
- Migration guide

** Timeline
| Phase | Start | End | Status |
|-------+-------+-----+--------|
| Phase 1 | [date] | [date] | In Progress |
| Phase 2 | [date] | [date] | Planned |

** Related Issues
- SRVKP-XXX1: [[link]]
- SRVKP-XXX2: [[link]]
```

### Meeting/Discussion Note
```org
* SRVKP-XXXX Discussion Notes

:PROPERTIES:
:JIRA: https://issues.redhat.com/browse/SRVKP-XXXX
:MEETING_DATE: [date]
:ATTENDEES: person1, person2, person3
:END:

** Context
[Issue being discussed]

** Discussion Points
*** Point 1
[Discussion and decisions]

*** Point 2
[Discussion and decisions]

** Decisions
- Decision 1
- Decision 2

** Action Items
- [ ] Action 1 - @person1
- [ ] Action 2 - @person2

** Next Steps
[What happens next]
```

## Workflow Integration

### From Jira to Note
1. User mentions Jira issue
2. Fetch issue details
3. Create denote note with structure
4. Populate with issue information
5. Add sections for work tracking

### From Note to Jira
1. User creates note about topic
2. Realize it needs Jira tracking
3. Create Jira issue with Notes skill context
4. Link note to newly created issue
5. Add Jira property to note

### Bi-directional Linking
1. Issue has note reference in comment
2. Note has Jira property
3. Easy navigation between both
4. Synchronized updates

## Org-Mode Properties for Jira

```org
:PROPERTIES:
:JIRA: https://issues.redhat.com/browse/ISSUE-KEY
:JIRA_KEY: SRVKP-1234
:JIRA_STATUS: In Progress
:JIRA_PRIORITY: Major
:JIRA_ASSIGNEE: username
:LAST_SYNCED: [timestamp]
:END:
```

## Benefits

1. **Rich Documentation**: More detailed than Jira allows
2. **Local Search**: Find issues via denote search
3. **Context Preservation**: Keep investigation notes
4. **Linking**: Connect issues with code, docs, other notes
5. **Version Control**: Track note changes over time
6. **Offline Access**: Work without internet
7. **Integration**: Part of your note-taking workflow

## Best Practices

1. **Create early**: Start note when beginning work
2. **Update regularly**: Keep notes current
3. **Link liberally**: Connect to related notes
4. **Use org features**: Tags, TODOs, timestamps
5. **Sync key info**: Update Jira with major findings
6. **Preserve context**: Don't delete investigation paths
7. **Structure consistently**: Use templates

## Follow-up Actions

After linking:
- **Add TODO**: If action needed
- **Update Jira**: Comment with note link
- **Tag appropriately**: For denote searching
- **Link related notes**: Cross-reference
- **Schedule review**: Revisit for updates

## Tips

- **Use denote backlinks**: Find all issues-related notes
- **Tag by project**: `tekton`, `pipelines`, etc.
- **Tag by type**: `bug`, `feature`, `investigation`
- **Include timestamps**: Track when work done
- **Export to Jira**: Copy sections to Jira when done
- **Archive when done**: Don't delete, archive

## Integration with Other Skills

- **Notes Skill**: Create and manage denote notes
- **TODOs Skill**: Create action items from issues
- **Git Skill**: Link commits to issues and notes
- **Org Skill**: Manage org-mode structure
