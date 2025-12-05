# Review Inbox Workflow

## Purpose
Analyze items in ~/desktop/org/inbox.org and propose appropriate refile targets in ~/desktop/org/todos.org.

## When to Use
- User asks to "review inbox"
- User wants refile suggestions for inbox items
- User says "analyze my inbox" or similar

## Workflow Steps

### 1. Read the Inbox
Read ~/desktop/org/inbox.org to get all current items.

### 2. Read the TODOs File Structure
Read ~/desktop/org/todos.org to understand:
- Available top-level sections (Work, Projects, Systems, Personal, Routines, Appointments, Health)
- Existing projects that might be relevant
- Current active areas of focus

### 3. Analyze Each Inbox Item
For each TODO or link in inbox, consider:

**Context clues:**
- Keywords (work, tekton, emacs, keyboard, homelab, etc.)
- Related systems mentioned (nixos, github, servers, etc.)
- Personal vs. work nature
- Whether it fits an existing project

**Refile targets:**
- **Work section** - Tekton, OpenShift Pipelines, upstream work, professional development
  - Check for existing related projects to nest under
- **Projects section** - Multi-step initiatives (Personal finance, Keyboard, Websites, etc.)
  - Group related items under existing projects when possible
- **Systems section** - Infrastructure, emacs config, homelab, nix, servers
  - Emacs configuration items go under "Emacs configuration cleanup" project
  - Skills/Claude items group together
  - Server/infrastructure tasks
- **Personal section** - Life admin, appointments, purchases
- **Routines section** - Only for recurring scheduled items
- **Health section** - Health-related tasks
- **Appointments section** - Specific time-based events

**Special cases:**
- Web links without context → Suggest archiving or adding to relevant project notes
- Items already done → Mark as DONE and suggest archiving
- Items that are really questions/research → May belong in notes instead of TODOs

### 4. Present Analysis
For each inbox item, provide:
1. **Item summary** (brief description)
2. **Proposed target** (section and optionally which project/heading)
3. **Reasoning** (why this location makes sense)
4. **Alternative** (if there's another reasonable option)

Format as a clear list:
```
1. "Item title/description"
   → Target: Work / A tool to manage pull-request from cli
   → Reason: GitHub/PR tooling, fits existing project

2. "Another item"
   → Target: Systems / Emacs configuration "cleanup"
   → Reason: Emacs feature enhancement
   → Alternative: Could also be standalone in Systems
```

### 5. Handle Special Items
- **Links** - Group together and suggest:
  - Archive if no longer relevant
  - Add to project notes if related to active work
  - Keep in inbox if needs review
- **Completed items** - Mark as DONE, suggest archiving
- **Ambiguous items** - Ask user for clarification before suggesting target

### 6. Offer Next Steps
After presenting suggestions:
- Ask if user wants to proceed with refiling
- Offer to do the refile operations (using Refile workflow)
- Or user can manually refile in Emacs

## Important Notes

**DO NOT automatically refile** - Only propose targets and wait for approval

**Context awareness:**
- Consider user's current focus areas (check recent todos.org activity)
- Look for natural groupings (multiple related items → same project)
- Respect existing project structure

**Properties to preserve:**
- CREATED timestamps
- LOGBOOK entries
- Any properties from inbox items

**After refiling:**
- Keep "From: [[file:inbox.org][inbox]]" links
- Add proper PROPERTIES drawer with :CREATED: timestamp
- Maintain original content and context

## Example Interaction

```
User: "Review my inbox and suggest where to refile things"
Claude: [Reads inbox.org and todos.org]

I found 10 items in your inbox. Here are my refile suggestions:

1. "Update aion's job" (sync folders, traefik, MQTT)
   → Target: Systems / Migrate aion project
   → Reason: Fits perfectly with existing aion migration work

2. "Figure out syncthing conflicts"
   → Target: Systems (standalone)
   → Reason: Infrastructure/syncthing task

3. "Create Python skill"
   → Target: Systems (group with other skills)
   → Reason: Groups with Rust, Android, org-mode skills

[... etc for all items ...]

Would you like me to proceed with refiling these items?
```

## Tips

- Group similar items (e.g., all skill creation tasks together)
- Look for existing projects before suggesting new standalone items
- Consider the user's workflow patterns (check SCHEDULED dates, active projects)
- When in doubt, ask for clarification rather than guessing
