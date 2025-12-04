# Add TODO Workflow

## Purpose
Add new TODO items to inbox.org or directly to todos.org sections.

## When to Use
- User wants to create a new TODO
- Capturing a task or action item
- Adding follow-up work
- Creating project tasks

## Decision: Inbox vs Direct

### Add to Inbox (Recommended for most cases)
Use inbox.org when:
- Quick capture during work
- Not sure which section it belongs to
- Want to process later
- Batch capturing multiple items

### Add Directly to Section
Use todos.org directly when:
- Clear which section (Work, Systems, Projects, Personal)
- Part of existing project
- Well-defined task with all details

## Quick Add to Inbox

### Basic Format
```org
* TODO <description>
```

### With Priority
```org
* TODO [#2] <description>
```

### With Context
```org
* TODO <description>
:PROPERTIES:
:CREATED:       [<current-date>]
:END:

<additional context or notes>
```

## Add Directly to Section

### 1. Identify the Correct Section

**Work Section** (`* Work`):
- Job-related tasks
- Upstream work
- Team collaboration
- Work meetings prep

**Projects Section** (`* Projects`):
- New projects or features
- Multi-step initiatives
- Both work and personal projects

**Systems Section** (`* Systems`):
- Homelab tasks
- Infrastructure changes
- NixOS configuration
- Network setup

**Personal Section** (`* Personal`):
- Life admin
- Appointments
- Personal errands

**Routines Section** (`* Routines`):
- Recurring meetings
- Regular check-ins
- Habit tracking

**Health Section** (`* Health`):
- Health appointments
- Exercise tracking
- Medical tasks

### 2. Choose the Right State

- **TODO**: Default for new tasks
- **NEXT**: If it's the next action to take
- **WAIT**: If waiting on something/someone

### 3. Set Priority (Optional)
- `[#1]` - Highest/Most important
- `[#2]` - High priority
- `[#3]` - Medium priority
- `[#4]` - Low priority
- `[#5]` - Lowest priority

### 4. Add Scheduling (Optional)

**SCHEDULED**: When to start
```org
SCHEDULED: <2025-12-05 Fri>
```

**DEADLINE**: When it must be done
```org
DEADLINE: <2025-12-10 Wed>
```

**Repeating**: For recurring tasks
```org
SCHEDULED: <2025-12-08 Mon ++1w>
```

## Templates

### Simple TODO
```org
** TODO <description>
```

### TODO with Priority
```org
** TODO [#2] <description>
```

### TODO with Scheduling
```org
** TODO <description>
SCHEDULED: <2025-12-05 Fri>
```

### TODO with Context
```org
** TODO [#3] <description>
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: work
:END:

<background information>
<links to relevant resources>
```

### Project with Subtasks
```org
** TODO <project-name> [0/3]
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:END:

*** TODO First subtask
*** TODO Second subtask
*** TODO Third subtask
```

### Recurring Task
```org
** TODO <description>
SCHEDULED: <2025-12-08 Mon ++1w>
:PROPERTIES:
:STYLE:    habit
:LAST_REPEAT: [2025-12-01 Mon 13:40]
:END:
```

## Workflow Steps

### Using Inbox (Quick Capture)

1. **Open inbox.org**
   ```bash
   # Location: ~/desktop/org/inbox.org
   ```

2. **Add Entry**
   ```bash
   # Append to end
   echo "* TODO Fix authentication bug" >> ~/desktop/org/inbox.org
   ```

3. **Refile Later**
   - During daily review
   - Use Refile workflow
   - Move to appropriate section

### Direct Add to Section

1. **Read Current Section**
   ```bash
   # Example: Read Work section
   sed -n '/^\* Work/,/^\* [A-Z]/p' ~/desktop/org/todos.org | tail -20
   ```

2. **Find Insertion Point**
   - After section header
   - Before archived items
   - Near related TODOs

3. **Format the Entry**
   - Use ** for level-2 heading
   - Add state (TODO, NEXT)
   - Add priority if needed
   - Add properties if needed
   - Add scheduling if needed

4. **Insert Entry**
   Use Edit tool to add at appropriate location

## Examples

### Example 1: Quick Inbox Capture
User asks: "Add a todo to review the upstream PR #123"

```bash
# Add to inbox
echo "* TODO Review upstream PR #123

https://github.com/org/repo/pull/123
" >> ~/desktop/org/inbox.org
```

### Example 2: Direct Work TODO
User asks: "Add a high priority work task to update documentation by Friday"

Find Work section, then insert:
```org
** TODO [#2] Update project documentation
DEADLINE: <2025-12-05 Fri>
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: work
:END:

Update getting started guide and API reference
```

### Example 3: System Configuration Task
User asks: "Add a todo to configure MQTT on rhea"

Find Systems section, then insert:
```org
** TODO [#3] Setup MQTT broker on rhea
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: systems
:END:

- Install mosquitto
- Configure authentication
- Setup firewall rules
```

### Example 4: New Project with Subtasks
User asks: "Create a project for keyboard firmware improvements"

Find Projects section, then insert:
```org
** TODO Keyboard firmware improvements [0/3]

*** TODO Implement leader keys
SCHEDULED: <2025-12-05 Fri>

*** TODO Standardize nav/media/mouse layers

*** TODO Add combos for common symbols
```

### Example 5: Recurring Meeting
User asks: "Add weekly 1:1 meeting every Monday at 10am"

Find Routines section, then insert:
```org
** STRT Weekly 1:1 with Manager
SCHEDULED: <2025-12-08 Mon 10:00 ++1w>
:PROPERTIES:
:LAST_REPEAT: [2025-12-01 Mon 10:00]
:CATEGORY: work
:END:

Meeting notes: <link-to-notes>
```

## Tips

1. **Default to inbox**: When in doubt, add to inbox and refile later
2. **Keep it brief**: You can add details when you start working on it
3. **Link resources**: Add URLs, file links immediately while you remember
4. **Set realistic dates**: Don't over-schedule
5. **Use NEXT sparingly**: Only for truly next actions
6. **Break down large tasks**: Create projects with subtasks
7. **Add creation date**: Helps track how long tasks sit
8. **Use categories**: Makes filtering and agenda views easier

## Common Patterns

### From Email/Message
"Add a todo to follow up with Alice about the deployment"
```org
* TODO Follow up with Alice about deployment
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:END:

Context from email: <paste relevant part>
```

### From Code Review
"Add a todo to refactor the authentication module"
```org
* TODO Refactor authentication module
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: work
:END:

See PR comments: https://github.com/org/repo/pull/456
```

### From Meeting
"Add a todo to prepare slides for next week's presentation"
```org
* TODO Prepare presentation slides
DEADLINE: <2025-12-10 Wed>
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: work
:END:

Topics to cover:
- Project overview
- Timeline
- Next steps
```

### From Note
"Add a todo based on this learning note"
```org
* TODO Apply learned Rust patterns to auth module
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:END:

From: [[file:~/desktop/org/notes/20251203T160000--learning-rust-ownership__learning_rust.org][Learning Rust Ownership]]
```

## Integration

### With Git Workflow
After committing, create follow-up task:
```bash
last_commit=$(git log -1 --oneline)
echo "* TODO Test changes from $last_commit" >> ~/desktop/org/inbox.org
```

### With Notes
Reference TODO in note:
```org
* Implementation

TODO created: [[file:~/desktop/org/todos.org::*Implement feature X][Implement feature X]]
```

### With Calendar
Convert calendar event to TODO:
```org
** TODO Prepare for team meeting
SCHEDULED: <2025-12-05 Fri 09:00>
```

## Validation

Before adding, check:
- [ ] Clear, actionable description
- [ ] Appropriate section (or inbox)
- [ ] Priority set if urgent
- [ ] Scheduling if time-sensitive
- [ ] Links to context/resources
- [ ] Not a duplicate

## Next Steps

After adding TODO:
1. If added to inbox → Plan to refile during review
2. If high priority → Consider marking as NEXT
3. If urgent → Set SCHEDULED or DEADLINE
4. If part of project → Link to project heading
5. If needs research → Link to relevant notes
