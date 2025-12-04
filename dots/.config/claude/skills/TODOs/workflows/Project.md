# Project TODO Workflow

## Purpose
Manage multi-step projects with subtasks, progress tracking, and dependencies.

## When to Use
- Task requires 3+ steps
- Multi-day or multi-week effort
- Complex feature or initiative
- Has clear deliverable

## Project vs Simple TODO

### Use Simple TODO When
- Single action required
- Can complete in one session
- No dependencies
- Straightforward task

### Use Project When
- Multiple related tasks
- Sequential or parallel work
- Track overall progress
- Coordinate related efforts

## Project Structure

### Basic Project Format
```org
** TODO Project Name [0/3]
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:END:

*** TODO First subtask
*** TODO Second subtask
*** TODO Third subtask
```

### Enhanced Project Format
```org
** TODO Project Name [0/3]
SCHEDULED: <2025-12-05 Fri>
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: work
:END:

Brief description of project goal and context.

*** TODO First subtask
SCHEDULED: <2025-12-05 Fri>

*** TODO Second subtask

*** TODO Third subtask
```

## Progress Tracking

### Progress Counters

**Format**: `[completed/total]`

**Automatic Updates** (if using Emacs):
- Updates when subtask marked DONE
- Reflects current progress

**Manual Updates** (command line):
```org
** TODO Keyboard improvements [1/3]

*** DONE Leader keys implementation
*** TODO Nav/media standardization
*** TODO Symbol combos
```

After completing another:
```org
** TODO Keyboard improvements [2/3]

*** DONE Leader keys implementation
*** DONE Nav/media standardization
*** TODO Symbol combos
```

### Percentage Tracking

Alternative to counters:
```org
** TODO Project Name [33%]

*** DONE First task
*** TODO Second task
*** TODO Third task
```

## Project Types

### Sequential Project
Tasks must be done in order:

```org
** TODO Setup Home Assistant [0/5]
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:END:

*** TODO Install Raspberry Pi OS
SCHEDULED: <2025-12-05 Fri>
Dependencies must complete first.

*** TODO Install Home Assistant
Depends on: Raspberry Pi OS installed

*** TODO Configure MQTT broker
Depends on: Home Assistant running

*** TODO Add first device
Depends on: MQTT configured

*** TODO Setup automations
Depends on: Devices connected
```

### Parallel Project
Tasks can be done independently:

```org
** TODO Website Redesign [0/4]

*** TODO Design new homepage
SCHEDULED: <2025-12-05 Fri>

*** TODO Write new content
SCHEDULED: <2025-12-05 Fri>

*** TODO Setup new hosting
SCHEDULED: <2025-12-06 Sat>

*** TODO Migrate DNS
(Do last, after all others complete)
```

### Research Project
Exploratory with evolving scope:

```org
** TODO Research backup solutions [0/4]
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:END:

*** TODO Evaluate restic
:LOGBOOK:
- Note taken on [2025-12-05 Fri 10:00] \\
  Good: encryption, deduplication, active development
  Bad: complex setup
:END:

*** TODO Evaluate borg backup

*** TODO Compare performance

*** DONE Create comparison matrix
CLOSED: [2025-12-04 Thu 17:00]
```

## Project Templates

### Feature Implementation
```org
** TODO Feature: User Authentication [0/5]
SCHEDULED: <2025-12-05 Fri>
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: work
:END:

Implement secure user authentication with JWT tokens.

*** TODO Design auth flow and API
SCHEDULED: <2025-12-05 Fri>

*** TODO Implement backend auth endpoints

*** TODO Create login UI components

*** TODO Write tests

*** TODO Update documentation
```

### Infrastructure Project
```org
** TODO Setup Kubernetes Cluster [0/6]
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: systems
:END:

*** TODO Provision VMs
SCHEDULED: <2025-12-06 Sat>

*** TODO Install k3s

*** TODO Configure networking

*** TODO Setup storage

*** TODO Deploy test application

*** TODO Document setup
```

### Learning Project
```org
** TODO Learn Rust Basics [0/8]
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: personal
:END:

Work through Rust book and build sample projects.

*** TODO Read chapters 1-5
SCHEDULED: <2025-12-05 Fri>

*** TODO Complete ownership exercises

*** TODO Build CLI tool

*** TODO Read chapters 6-10

*** TODO Build web service

*** TODO Learn async programming

*** TODO Contribute to open source

*** TODO Write blog post about learning
```

### Personal Project
```org
** TODO Organize Home Office [0/5]
DEADLINE: <2025-12-15 Mon>
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:CATEGORY: personal
:END:

*** TODO Declutter desk
SCHEDULED: <2025-12-07 Sun>

*** TODO Organize cables

*** TODO Setup better lighting

*** TODO Arrange bookshelf

*** TODO Add plants
```

## Managing Projects

### Starting a Project

1. **Define the goal**
   - What's the deliverable?
   - Why are we doing this?
   - What's the scope?

2. **Break down into subtasks**
   - 3-7 subtasks ideal
   - Too many? Create sub-projects
   - Too few? Might not need project

3. **Identify dependencies**
   - What must happen first?
   - What can be parallel?
   - Any blockers?

4. **Schedule first task**
   - When to start?
   - Set SCHEDULED on project and/or first subtask

5. **Add context**
   - Links to docs, designs, notes
   - Related issues or PRs
   - Background information

### Working on a Project

1. **Review project regularly**
   - Daily if active
   - Weekly if background

2. **Mark subtasks DONE as completed**
   - Update progress counter
   - Add completion notes

3. **Add new subtasks as discovered**
   - Projects evolve
   - Increment total counter

4. **Track progress notes**
   ```org
   :LOGBOOK:
   - Note taken on [2025-12-05 Fri 14:00] \\
     Completed initial setup. Performance better than expected.
     Next: start on configuration phase.
   :END:
   ```

5. **Reschedule if needed**
   - Be honest about timeline
   - Adjust SCHEDULED dates
   - Note reasons for delays

### Completing a Project

1. **Mark all subtasks DONE**
   ```org
   *** DONE All subtasks
   ```

2. **Mark project DONE**
   ```org
   ** DONE Project Name [3/3]
   CLOSED: [2025-12-10 Wed 15:30]
   ```

3. **Add completion note**
   ```org
   :LOGBOOK:
   - State "DONE"       from "STRT"       [2025-12-10 Wed 15:30] \\
     Project completed successfully. All features working as expected.
     Deployed to production.
   :END:
   ```

4. **Create follow-up tasks if needed**
   ```org
   ** TODO Monitor new feature performance
   SCHEDULED: <2025-12-17 Wed>

   Follow-up from: [[*Feature: User Authentication][User Authentication Project]]
   ```

5. **Archive during weekly review**

## Project Lifecycle

### Phase 1: Planning
```org
** TODO Project Name [0/0]
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:END:

Planning phase - breaking down into subtasks.

* Planning Notes
- Goal: ...
- Approach: ...
- Resources needed: ...
```

### Phase 2: Active Work
```org
** STRT Project Name [2/5]
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:END:

*** DONE Subtask 1
*** DONE Subtask 2
*** STRT Subtask 3
*** TODO Subtask 4
*** TODO Subtask 5
```

### Phase 3: Completion
```org
** DONE Project Name [5/5]
CLOSED: [2025-12-15 Mon 10:30]
:LOGBOOK:
- State "DONE"       from "STRT"       [2025-12-15 Mon 10:30]
:END:

*** DONE Subtask 1-5 (all complete)
```

## Advanced Patterns

### Nested Projects
```org
** TODO Large Initiative [0/3]

*** TODO Phase 1: Research [0/3]

**** TODO Literature review
**** TODO Prototype evaluation
**** TODO Decision document

*** TODO Phase 2: Implementation [0/4]

**** TODO Setup infrastructure
**** TODO Core features
**** TODO Testing
**** TODO Documentation

*** TODO Phase 3: Rollout [0/2]

**** TODO Pilot deployment
**** TODO Full deployment
```

### Project with External Dependencies
```org
** WAIT Website Migration [1/4]
:PROPERTIES:
:CREATED:       [2025-12-04 Thu 15:30]
:END:
:LOGBOOK:
- State "WAIT"       from "STRT"       [2025-12-08 Mon 10:00] \\
  Waiting for DNS propagation (24-48 hours)
:END:

*** DONE Export content from old site
*** TODO Setup new hosting (BLOCKED: waiting for DNS)
*** TODO Import content
*** TODO Verify and go live
```

### Milestone Tracking
```org
** TODO Product Launch [3/10]
DEADLINE: <2025-12-31 Wed>

*** DONE Milestone 1: Design Complete [3/3]
CLOSED: [2025-11-15 Mon]

**** DONE UI mockups
**** DONE User flow
**** DONE Design review

*** STRT Milestone 2: MVP Development [2/4]

**** DONE Core features
**** DONE Basic UI
**** TODO Testing
**** TODO Bug fixes

*** TODO Milestone 3: Launch Prep [0/3]

**** TODO Marketing materials
**** TODO Documentation
**** TODO Launch plan
```

## Tips

1. **Right-size projects**: 3-7 subtasks is ideal
2. **Break down large projects**: Use nested structure
3. **Update progress regularly**: Keep counters current
4. **Schedule first task**: Gets momentum going
5. **Track dependencies**: Note blockers explicitly
6. **Add context liberally**: Links, notes, decisions
7. **Review weekly**: Keep projects moving
8. **Don't abandon**: Either complete or cancel
9. **Celebrate completion**: Mark milestone reached

## Common Patterns

### From Idea to Project
```org
# Start: Simple idea
* TODO Build personal dashboard

# Evolve: Realize it's bigger
** TODO Build personal dashboard [0/4]

*** TODO Research frameworks
*** TODO Design layout
*** TODO Implement data sources
*** TODO Deploy to homelab
```

### Project Spawns Follow-up
```org
# Original project completes
** DONE Setup Home Assistant [5/5]

# Creates new project
** TODO Home Automation Expansion [0/3]
Based on learnings from initial Home Assistant setup.

*** TODO Add more sensors
*** TODO Create advanced automations
*** TODO Integrate with other services
```

### Project Gets Cancelled
```org
** CANX Migrate to new framework [2/5]
CLOSED: [2025-12-08 Mon 15:00]
:LOGBOOK:
- State "CANX"       from "STRT"       [2025-12-08 Mon 15:00] \\
  Team decided to stick with current framework after evaluation.
  Completed research was valuable even though not proceeding.
:END:

*** DONE Research alternatives
*** DONE Create comparison matrix
*** CANX Setup test environment
*** CANX Migration plan
*** CANX Execute migration
```

## Validation Checklist

Good project has:
- [ ] Clear goal and deliverable
- [ ] 3-7 subtasks (not too few, not too many)
- [ ] Progress counter [n/m]
- [ ] CREATED timestamp
- [ ] Context and links
- [ ] First task scheduled (if starting now)
- [ ] Dependencies noted if sequential

## Integration

### Link to Notes
```org
** TODO Project Name [0/3]

Design doc: [[file:~/desktop/org/notes/20251204--project-design__work.org][Project Design Note]]
```

### Link to Code
```org
** TODO Refactor Authentication [1/3]

Codebase: [[file:~/src/myproject/src/auth/][Auth Module]]
```

### Reference from Other TODOs
```org
** TODO Deploy changes

Depends on: [[*Project Name][Project Name]] completion
```

## Anti-Patterns

❌ **Too many subtasks**: >10 means break into sub-projects
❌ **Too few subtasks**: <3 might not need project structure
❌ **Vague subtasks**: "Do the thing" isn't actionable
❌ **Never update progress**: Counters get stale
❌ **Abandon without cancelling**: Either finish or mark CANX
❌ **No scheduling**: Projects drift without dates
✅ **Right-sized**: 3-7 clear subtasks
✅ **Regular updates**: Progress tracked weekly
✅ **Clear dependencies**: Know what blocks what
✅ **Scheduled start**: First task has date
✅ **Complete or cancel**: Honest about status
