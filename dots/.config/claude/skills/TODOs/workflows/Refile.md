# Refile TODO Workflow

## Purpose
Move TODOs from inbox to proper sections in todos.org, or reorganize between sections.

## When to Use
- Processing inbox.org items
- Moving TODOs to correct section
- Reorganizing as priorities change
- Converting quick captures to structured TODOs

## Refile Process

### 1. Review Inbox Items
```bash
# Show current inbox
cat ~/desktop/org/inbox.org

# Count items
grep -c "^\* TODO" ~/desktop/org/inbox.org
```

### 2. For Each Item, Determine Section

**Work Section**: Job-related, upstream, team collaboration
**Projects Section**: Multi-step initiatives, features
**Systems Section**: Homelab, infrastructure, NixOS
**Personal Section**: Life admin, errands, appointments
**Routines Section**: Recurring meetings, habits
**Health Section**: Health appointments, tracking

### 3. Add Appropriate Properties

When refiling, enhance the TODO:
- Add CREATED timestamp if missing
- Set CATEGORY property
- Add priority if important
- Set SCHEDULED or DEADLINE if time-sensitive
- Add context notes

### 4. Remove from Inbox

After successfully refiling to todos.org, remove from inbox.org.

## Refile Templates

### From Simple Inbox Entry
**Inbox**:
```org
* TODO Review PR #123
```

**Refiled to Work**:
```org
** TODO Review PR #123
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: work
:END:

https://github.com/tektoncd/pipeline/pull/123
```

### From Quick Capture to Scheduled Task
**Inbox**:
```org
* TODO Setup MQTT broker
```

**Refiled to Systems**:
```org
** TODO [#3] Setup MQTT broker on rhea
SCHEDULED: <2025-12-06 Fri>
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: systems
:END:

- Install mosquitto
- Configure authentication
- Setup firewall rules
```

### From Note to Project
**Inbox**:
```org
* TODO Improve keyboard firmware
```

**Refiled to Projects**:
```org
** TODO Keyboard firmware improvements [0/3]
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:END:

*** TODO Implement leader keys
SCHEDULED: <2025-12-05 Fri>

*** TODO Standardize nav/media layers

*** TODO Add symbol combos
```

## Decision Matrix

### Which Section?

| Content | Section | Example |
|---------|---------|---------|
| Upstream work, team tasks | Work | "Review CLI roadmap" |
| Multi-step feature/goal | Projects | "Setup home automation" |
| Infrastructure, configs | Systems | "Configure wireguard VPN" |
| Life admin, personal | Personal | "Schedule dentist" |
| Recurring events | Routines | "Weekly team meeting" |
| Health-related | Health | "Exercise tracking" |

### Priority Assignment

| Urgency | Priority | When |
|---------|----------|------|
| Critical/Urgent | [#1] | Must do today/tomorrow |
| Important | [#2] | Should do this week |
| Normal | [#3] | Regular work |
| Low | [#4] | Nice to have |
| Someday | [#5] | Future consideration |

### Scheduling Guidelines

**Set SCHEDULED when**:
- You know when to start
- Depends on specific date
- Part of time-blocked plan

**Set DEADLINE when**:
- Hard due date exists
- External commitment
- Time-sensitive

**Leave unscheduled when**:
- Flexible timing
- Backlog item
- Waiting for dependencies

## Workflow Steps

### Daily Inbox Processing (5 minutes)

1. **Open inbox**
   ```bash
   cat ~/desktop/org/inbox.org
   ```

2. **For each item**:
   - Decide section
   - Add properties
   - Set priority/schedule if needed
   - Find insertion point in todos.org
   - Insert using Edit tool
   - Remove from inbox

3. **Clear processed items**
   Update inbox.org to remove refiled items

### Weekly Inbox Cleanup (10 minutes)

1. **Review all inbox items**
2. **Batch process by section**:
   - All work items → Work section
   - All systems items → Systems section
   - etc.
3. **Archive or delete**:
   - Old items no longer relevant
   - Duplicates
   - Already done

## Examples

### Example 1: Simple Work Task
**Inbox item**:
```org
* TODO Fix bug in authentication
```

**Process**:
1. Identify: Work section (job-related)
2. Enhance: Add priority, properties
3. Insert after Work header:
```org
** TODO [#2] Fix bug in authentication
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: work
:END:

Reported issue with null pointer in validation
```
4. Remove from inbox

### Example 2: Multi-Part Project
**Inbox item**:
```org
* TODO Setup backup system
```

**Process**:
1. Identify: Systems section (infrastructure)
2. Break down into project with subtasks
3. Insert in Systems:
```org
** TODO Setup automated backups [0/4]
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: systems
:END:

*** TODO Choose backup tool (restic vs borg)
*** TODO Configure backup schedules
*** TODO Setup remote storage
*** TODO Test restore procedure
```
4. Remove from inbox

### Example 3: Time-Sensitive Personal Task
**Inbox item**:
```org
* TODO Renew car insurance
```

**Process**:
1. Identify: Personal section
2. Add deadline (expires Dec 15)
3. Set priority (important)
4. Insert in Personal:
```org
** TODO [#2] Renew car insurance
DEADLINE: <2025-12-15 Mon>
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: personal
:END:

Policy expires Dec 15
Compare quotes before renewing
```
5. Remove from inbox

### Example 4: Recurring Event
**Inbox item**:
```org
* TODO Weekly team sync
```

**Process**:
1. Identify: Routines section
2. Set recurring schedule
3. Insert in Routines:
```org
** STRT Weekly team sync
SCHEDULED: <2025-12-05 Fri 14:00 ++1w>
:PROPERTIES:
:LAST_REPEAT: [2025-11-28 Fri 14:00]
:CATEGORY: work
:END:

Meeting link: https://meet.google.com/...
```
5. Remove from inbox

### Example 5: Reorganize Between Sections
**Current location** (in Work):
```org
** TODO Setup home automation system
```

**Should be in Projects**:
1. Read current TODO from Work section
2. Decide it's actually a project
3. Expand into project with subtasks
4. Insert in Projects section:
```org
** TODO Home automation with Home Assistant [0/5]
:PROPERTIES:
:CREATED:       [2025-11-15 Mon 10:00]
:END:

*** TODO Setup Home Assistant on Raspberry Pi
*** TODO Configure MQTT broker
*** TODO Add temperature sensors
*** TODO Setup automations
*** TODO Mobile app configuration
```
5. Remove from Work section

## Batch Refiling

### Process Multiple Work Items
```bash
# Show all work-related in inbox
grep -A2 "TODO.*\(PR\|review\|upstream\|team\)" ~/desktop/org/inbox.org

# Refile each to Work section
# Then clear from inbox
```

### Process Multiple System Items
```bash
# Show all system-related in inbox
grep -A2 "TODO.*\(setup\|configure\|deploy\|nixos\)" ~/desktop/org/inbox.org

# Refile each to Systems section
```

## Tips

1. **Process inbox regularly**: Daily keeps it manageable
2. **Don't overthink**: When unsure, pick best guess and move on
3. **Enhance while refiling**: Add context, links, notes
4. **Break down large items**: Convert to projects
5. **Delete ruthlessly**: Old captures may no longer be relevant
6. **Link to source**: Add links to emails, notes, issues
7. **Set realistic dates**: Don't over-schedule
8. **Use categories**: Helps with filtering later

## Common Patterns

### Email to TODO
Capture from email → inbox → refile to Work with context:
```org
** TODO Follow up on deployment discussion
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: work
:END:

From email thread with Alice
Need to schedule deployment for staging environment
```

### Meeting Action Item
Capture in meeting → inbox → refile with deadline:
```org
** TODO Prepare architecture presentation
DEADLINE: <2025-12-10 Wed>
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: work
:END:

Action item from team meeting
Present new architecture proposal
```

### Idea to Project
Random idea → inbox → refile as project:
```org
** TODO Build personal dashboard [0/4]
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:END:

*** TODO Research dashboard frameworks
*** TODO Design layout and widgets
*** TODO Implement data sources
*** TODO Deploy to homelab
```

## Validation Checklist

Before marking as refiled:
- [ ] Inserted in correct section
- [ ] Added CREATED timestamp
- [ ] Set CATEGORY property
- [ ] Priority set if important
- [ ] Scheduled/deadline if time-sensitive
- [ ] Context and links added
- [ ] Removed from inbox

## Integration

### With Email
```bash
# Create inbox entry from email
echo "* TODO Follow up with $NAME
From: $EMAIL_SUBJECT
" >> ~/desktop/org/inbox.org
```

### With Git
```bash
# Create inbox entry after finding issue
echo "* TODO Fix issue #$ISSUE_NUM
$(git log --oneline | head -1)
" >> ~/desktop/org/inbox.org
```

### With Notes
Link refiled TODO to note:
```org
** TODO Implement new feature
From: [[file:~/desktop/org/notes/20251204--feature-design__work.org][Feature Design]]
```

## Avoiding Common Mistakes

❌ **Don't leave in inbox forever**: Process regularly
❌ **Don't create duplicates**: Check if already exists in todos.org
❌ **Don't lose context**: Add links and notes while refiling
❌ **Don't over-elaborate**: Can add details later when working on it
❌ **Don't skip properties**: At least add CREATED and CATEGORY
✅ **Do process in batches**: Faster than one-by-one
✅ **Do enhance while refiling**: Better than bare TODO
✅ **Do delete old items**: Inbox is not a graveyard
✅ **Do link sources**: Email, notes, issues, PRs
