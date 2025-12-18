# Jira Skill Integration Guide

Deep integration patterns between Jira and your existing workflow tools.

## Integration Tools

### 1. jira-to-todo
Convert Jira issues to org-mode TODOs

**Features**:
- Single or multiple issue conversion
- JQL query support
- Automatic priority mapping (Jira → Org)
- Automatic state mapping (Jira status → TODO state)
- Add to inbox.org or output to file
- Preserves Jira metadata in properties

**Usage**:
```bash
# Convert single issue
jira-to-todo SRVKP-1234

# Convert my sprint issues
jira-to-todo --jql "sprint in openSprints() AND assignee = currentUser()"

# Add to inbox for refiling
jira-to-todo --jql "..." --add

# Save to file
jira-to-todo SRVKP-1234 --output ~/notes/issue-todo.org
```

**Priority Mapping**:
- Blocker/Critical → [#1]
- Major/High → [#2]
- Medium → [#3]
- Minor → [#4]
- Trivial → [#5]

**State Mapping**:
- To Do → TODO
- In Progress/Code Review/QE Review → STRT
- Done → DONE
- (Others) → TODO

### 2. jira-sprint-summary
Generate sprint summaries for standups, retrospectives, and planning

**Features**:
- Current/previous/next sprint selection
- Multiple report types (standup, retro, planning)
- Multiple output formats (text, org, md)
- Automatic metrics calculation
- Issue breakdowns by status and type

**Usage**:
```bash
# Daily standup notes
jira-sprint-summary --standup

# Retrospective in org format
jira-sprint-summary --prev --retro --format org

# Save planning notes
jira-sprint-summary --next --planning --output ~/notes/sprint-plan.org

# Current sprint summary
jira-sprint-summary --current
```

### 3. jira-search
Predefined search patterns (already covered in README)

### 4. jira-to-org
Convert issues to denote note format (already covered)

## Integration Patterns

### Pattern 1: Sprint → TODOs Workflow

**Scenario**: New sprint starts, create TODOs for your sprint commitments

```bash
# 1. Get current sprint issues
jira-sprint-summary --current

# 2. Convert sprint issues to TODOs
jira-to-todo --jql "sprint in openSprints() AND assignee = currentUser()" \
  --add --section Work

# 3. Review in Emacs and refile to appropriate sections
emacs ~/desktop/org/inbox.org
```

**Integration Points**:
- Jira sprint → org-mode TODOs
- Automatic priority/state mapping
- Preserves Jira links in properties
- Daily sync capability

### Pattern 2: Issue → Note → TODO Workflow

**Scenario**: Complex issue requiring investigation and tracking

```bash
# 1. View issue
jira issue view SRVKP-7327

# 2. Create investigation note
jira-to-org SRVKP-7327 --template investigation \
  --output ~/desktop/org/notes/$(date +%Y%m%dT%H%M%S)--srvkp-7327-affinity-assistant__jira_tekton_bug.org

# 3. Create TODO
jira-to-todo SRVKP-7327 --add --section Work

# 4. Start work
jira issue assign SRVKP-7327 $(jira me)
jira issue move SRVKP-7327 "In Progress"

# 5. Work in note, update Jira with findings
# (In Emacs, edit note)

# 6. Update Jira
jira issue comment add SRVKP-7327 "$(cat <<EOF
Root cause identified: [details from note]

Investigation notes: ~/desktop/org/notes/[note-file]
EOF
)"

# 7. Complete
jira issue move SRVKP-7327 "Code Review"
# Update TODO state in org-mode
```

**Integration Points**:
- Jira → denote note with structure
- Jira → org TODO
- Note ↔ Jira bidirectional references
- Manual sync of states

### Pattern 3: Daily Standup Workflow

**Scenario**: Prepare for daily standup

```bash
# 1. Generate standup notes
jira-sprint-summary --standup --format org \
  --output ~/desktop/org/notes/$(date +%Y%m%dT%H%M%S)--daily-standup__standup_work.org

# 2. Review in Emacs
emacs ~/desktop/org/notes/*standup*.org

# 3. During standup, reference the note

# 4. After standup, update Jira states based on discussion
# (Use Transition workflow)
```

**Integration Points**:
- Jira → standup note
- Automatic yesterday/today/blockers extraction
- Sprint progress visibility
- Action item tracking

### Pattern 4: Sprint Planning Workflow

**Scenario**: Plan next sprint

```bash
# 1. Review previous sprint
jira-sprint-summary --prev --retro --format org \
  --output ~/desktop/org/notes/$(date +%Y%m%dT%H%M%S)--sprint-retro__sprint_tekton.org

# 2. Generate planning notes
jira-sprint-summary --next --planning --format org \
  --output ~/desktop/org/notes/$(date +%Y%m%dT%H%M%S)--sprint-planning__sprint_tekton.org

# 3. Add high priority items to sprint
# (View planning notes, then add to sprint)
SPRINT_ID=$(jira sprint list --next --plain | head -1 | awk '{print $1}')
jira sprint add "$SPRINT_ID" SRVKP-1234 SRVKP-5678

# 4. Create TODOs for sprint commitments
jira-to-todo --jql "sprint = $SPRINT_ID AND assignee = currentUser()" --add

# 5. In Emacs, organize TODOs and add to calendar
```

**Integration Points**:
- Previous sprint retrospective
- Planning notes generation
- High priority backlog visibility
- Sprint → TODOs conversion
- Calendar integration (manual)

### Pattern 5: Bug Triage Workflow

**Scenario**: Weekly bug triage session

```bash
# 1. Find unassigned bugs
jira-search team-bugs --limit 20 > /tmp/bugs.txt

# 2. Review and assign
cat /tmp/bugs.txt
# For each bug you take:
jira issue assign SRVKP-XXXX $(jira me)
jira-to-todo SRVKP-XXXX --add --section Work

# 3. Create triage notes
cat > ~/desktop/org/notes/$(date +%Y%m%dT%H%M%S)--bug-triage__triage_tekton.org <<EOF
* Bug Triage - $(date +%Y-%m-%d)

** Assigned to Me
$(jira issue list --jql "type = Bug AND assignee = currentUser() AND created >= -7d" --plain | sed 's/^/- /')

** Deferred
- [List of bugs deferred with reason]

** Action Items
- [ ] Follow up on critical bugs
- [ ] Update priorities
EOF
```

**Integration Points**:
- Jira search → file output
- Assignment + TODO creation
- Triage session notes
- Action item tracking

### Pattern 6: Weekly Review Workflow

**Scenario**: End of week review

```bash
# 1. What did I accomplish this week?
jira-search my-week --plain > ~/weekly-work.txt

# 2. Sprint progress
jira-sprint-summary --current --format org \
  --output ~/desktop/org/notes/$(date +%Y%m%dT%H%M%S)--weekly-review__review_work.org

# 3. Create weekly review note
cat >> ~/desktop/org/notes/*weekly-review*.org <<EOF

** Completed This Week
$(jira issue list --jql "assignee = currentUser() AND status changed to Done DURING (startOfWeek(), now())" --plain | sed 's/^/- DONE /')

** In Progress
$(jira-search my-progress --plain | sed 's/^/- STRT /')

** Next Week Focus
- [ ]
- [ ]

** Issues Encountered
-

** Notes
-
EOF

# 4. Review in Emacs and plan next week
```

**Integration Points**:
- Weekly accomplishments from Jira
- Sprint summary
- Weekly review note
- Planning for next week

## Automation Ideas

### 1. Auto-sync TODO States

**Cron job** to sync Jira → org-mode:
```bash
#!/bin/bash
# ~/bin/jira-todo-sync

# Get all Jira issues mentioned in org files
grep -rho "SRVKP-[0-9]*" ~/desktop/org/*.org | sort -u | while read issue; do
  # Get Jira status
  status=$(jira issue view "$issue" --plain 2>/dev/null | grep -i "status" | awk '{print $2}')

  # Map to org state
  case "$status" in
    Done) org_state="DONE" ;;
    "In Progress"|"Code Review") org_state="STRT" ;;
    "To Do") org_state="TODO" ;;
    *) org_state="" ;;
  esac

  # Update org file (requires custom org-mode function)
  # emacsclient --eval "(update-todo-by-jira \"$issue\" \"$org_state\")"
done
```

**Cron schedule**:
```
# Sync every morning at 8 AM
0 8 * * 1-5 ~/bin/jira-todo-sync
```

### 2. Daily Standup Note Generator

**Cron job** to generate daily standup notes:
```bash
#!/bin/bash
# ~/bin/generate-standup

NOTE_FILE=~/desktop/org/notes/$(date +%Y%m%dT%H%M%S)--daily-standup__standup_work.org

jira-sprint-summary --standup --format org --output "$NOTE_FILE"

# Optionally send notification
notify-send "Standup Notes Ready" "Generated: $NOTE_FILE"
```

**Cron schedule**:
```
# Generate standup notes at 8:30 AM on weekdays
30 8 * * 1-5 ~/bin/generate-standup
```

### 3. Sprint Start Automation

**Script** to run at sprint start:
```bash
#!/bin/bash
# ~/bin/sprint-start

# Get sprint info
SPRINT_ID=$(jira sprint list --current --plain | head -1 | awk '{print $1}')
SPRINT_NAME=$(jira sprint list --current | head -1)

# Create sprint planning note
NOTE_FILE=~/desktop/org/notes/$(date +%Y%m%dT%H%M%S)--sprint-start-${SPRINT_ID}__sprint_tekton.org

jira-sprint-summary --current --planning --format org --output "$NOTE_FILE"

# Create TODOs for sprint
jira-to-todo --jql "sprint in openSprints() AND assignee = currentUser()" --add

echo "Sprint started: $SPRINT_NAME"
echo "Planning note: $NOTE_FILE"
echo "TODOs added to inbox.org"
```

### 4. Weekly Retrospective

**Script** to run weekly:
```bash
#!/bin/bash
# ~/bin/weekly-retro

# Last week's work
RETRO_FILE=~/desktop/org/notes/$(date +%Y%m%dT%H%M%S)--weekly-retro__retro_work.org

cat > "$RETRO_FILE" <<EOF
* Weekly Retrospective - $(date +%Y-%m-%d)

** Completed This Week
$(jira issue list --jql "assignee = currentUser() AND status changed to Done DURING (startOfWeek(), now())" --plain | sed 's/^/- /')

** Sprint Progress
$(jira-sprint-summary --current)

** What Went Well


** What Could Be Improved


** Action Items for Next Week
- [ ]

** Notes

EOF

echo "Retrospective note created: $RETRO_FILE"
```

**Cron schedule**:
```
# Every Friday at 4 PM
0 16 * * 5 ~/bin/weekly-retro
```

## Integration with Existing Skills

### TODOs Skill

**Bidirectional Integration**:
```elisp
;; In Emacs, function to fetch Jira status for TODO
(defun my/sync-jira-todo ()
  "Sync Jira status to current TODO"
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (when-let ((jira-key (org-entry-get nil "JIRA_KEY")))
      (let* ((status (shell-command-to-string
                      (format "jira issue view %s --plain | grep -i status | awk '{print $2}'" jira-key)))
             (org-state (pcase (string-trim status)
                          ("Done" "DONE")
                          ((or "In Progress" "Code Review" "QE Review") "STRT")
                          ("To Do" "TODO")
                          (_ nil))))
        (when org-state
          (org-todo org-state)
          (message "Synced %s: %s -> %s" jira-key status org-state))))))
```

### Notes Skill

**Create Note with Jira Context**:
When creating a denote note, automatically add Jira properties if issue mentioned:
```elisp
(defun my/denote-with-jira ()
  "Create denote note with Jira issue context"
  (interactive)
  (let ((issue-key (read-string "Jira issue (optional): ")))
    (when (and issue-key (not (string-empty-p issue-key)))
      ;; Fetch issue details and populate note
      (shell-command (format "jira-to-org %s" issue-key)))))
```

### Git Skill

**Auto-reference in Commits**:
```bash
# Git prepare-commit-msg hook
#!/bin/bash
# .git/hooks/prepare-commit-msg

# Extract branch name
BRANCH=$(git symbolic-ref --short HEAD)

# If branch contains Jira key, add to commit message
if [[ $BRANCH =~ (SRVKP-[0-9]+) ]]; then
  JIRA_KEY="${BASH_REMATCH[1]}"
  # Add Jira reference if not already present
  if ! grep -q "$JIRA_KEY" "$1"; then
    echo "" >> "$1"
    echo "Refs: $JIRA_KEY" >> "$1"
  fi
fi
```

### Email Skill

**Send Sprint Summary via Email**:
```bash
#!/bin/bash
# ~/bin/email-sprint-summary

SUMMARY=$(jira-sprint-summary --current)

# Use email skill or sendmail
cat <<EOF | mail -s "Sprint Summary - $(date +%Y-%m-%d)" team@example.com
$SUMMARY

--
Generated by jira-sprint-summary
EOF
```

## Keyboard Shortcuts (Emacs)

Add to your Emacs config:
```elisp
;; Jira integration shortcuts
(defun my/jira-view-at-point ()
  "View Jira issue at point"
  (interactive)
  (when-let ((issue-key (thing-at-point 'symbol)))
    (when (string-match "SRVKP-[0-9]+" issue-key)
      (shell-command (format "jira issue view %s" issue-key)))))

(defun my/jira-create-todo ()
  "Create TODO from Jira issue at point"
  (interactive)
  (when-let ((issue-key (thing-at-point 'symbol)))
    (when (string-match "SRVKP-[0-9]+" issue-key)
      (shell-command (format "jira-to-todo %s --add" issue-key))
      (message "Created TODO for %s" issue-key))))

(defun my/jira-sync-current-todo ()
  "Sync current TODO with Jira"
  (interactive)
  (my/sync-jira-todo))

;; Keybindings
(global-set-key (kbd "C-c j v") 'my/jira-view-at-point)
(global-set-key (kbd "C-c j t") 'my/jira-create-todo)
(global-set-key (kbd "C-c j s") 'my/jira-sync-current-todo)
```

## Best Practices

1. **Regular Sync**: Run jira-todo-sync daily or before standup
2. **Note References**: Always link Jira issues in notes
3. **Consistent Workflow**: Follow established patterns
4. **Automation**: Use cron for regular tasks
5. **Documentation**: Document custom integrations
6. **Backup**: Keep notes in version control
7. **Review**: Weekly review of integration effectiveness

## Troubleshooting

### TODOs Out of Sync
Run manual sync:
```bash
jira-to-todo --jql "assignee = currentUser() AND status != Done" --add
```

### Missing Sprint Data
Verify sprint exists and you have access:
```bash
jira sprint list --current
jira sprint list --prev
```

### Integration Scripts Failing
Check:
- jira command is in PATH
- API token is valid
- JQL queries are correct
- File paths exist
- Permissions are correct

## Future Enhancements

1. **Real-time Sync**: Watch for Jira webhooks
2. **Conflict Resolution**: Handle divergent states
3. **Bulk Operations**: Batch sync multiple issues
4. **Calendar Integration**: Add sprint events to calendar
5. **Dashboard**: Terminal dashboard with live data
6. **Mobile Integration**: Phone notifications for updates
7. **AI Summaries**: LLM-generated sprint summaries
8. **Metrics Tracking**: Historical velocity and trends
