# History System

## Overview

The History System captures all significant work and learnings for future reference, analysis, and continuous improvement. It integrates with the denote note-taking system to provide both structured automation and manual curation.

Adapted from the [Personal AI Infrastructure Universal Output Capture System](https://github.com/danielmiessler/Personal_AI_Infrastructure/blob/main/.claude/skills/CORE/history-system.md).

## File Formats

History entries use **Markdown** (`.md`) format:
- Simple, universal format
- Git-friendly diffs
- Easy to read in any editor
- Standard markdown links: `[text](path)`

Denote notes use **Org-mode** (`.org`) format:
- Rich structure and features
- Emacs integration
- Org-mode links: `[[file:path][text]]`

**Why both?** Different tools for different purposes:
- History: Chronological logs, simple markdown
- Notes: Topic-based knowledge, org-mode features (TODO, tags, etc.)
- Both are plain text and searchable with grep/rg

## Core Principles

1. **Capture Everything**: Document all valuable work automatically and manually
2. **Zero Friction**: Make capturing history as easy as possible
3. **Searchable**: All history is text-based and grep-friendly
4. **Interconnected**: Link history entries with denote notes
5. **Categorized**: Organize by type for easy discovery

## Directory Structure

### History Directory
```
~/.config/claude/history/
├── sessions/                 # Work session summaries
│   └── YYYY-MM/
│       └── YYYY-MM-DD-HHMMSS_SESSION_description.md
│
├── learnings/                # Problem-solving insights
│   └── YYYY-MM/
│       └── YYYY-MM-DD-HHMMSS_LEARNING_description.md
│
├── research/                 # Deep investigations
│   └── YYYY-MM-DD_topic/
│       ├── analysis.md
│       ├── findings.md
│       └── sources.md
│
├── execution/                # Command outputs
│   └── YYYY-MM/
│       └── YYYY-MM-DD-HHMMSS_command-name.txt
│
└── decisions/                # Architecture decisions
    └── YYYY-MM/
        └── YYYY-MM-DD-HHMMSS_DECISION_description.md
```

### Notes Directory
```
~/desktop/org/notes/
├── YYYYMMDDTHHMMSS--title__tags.org     # Regular notes
└── YYYYMMDDTHHMMSS--title__history_*.org # History-linked notes
```

## Integration Strategy

### History Tag System

All history-related notes in the denote system use the `:history:` tag, plus category-specific tags:

- `:history:session:` - Session summaries
- `:history:learning:` - Learnings and insights
- `:history:research:` - Research findings
- `:history:decision:` - Architecture decisions
- `:history:execution:` - Command execution logs

### Bidirectional Links

**From History to Notes**:
History entries in `~/.config/claude/history/` can reference notes (using markdown links):
```markdown
## Related Notes
- [PAI Implementation](~/desktop/org/notes/20251203T151822--personal-ai-infrastructure__ai_claude.org)
```

**From Notes to History**:
Notes can reference history entries:
```org
* Related History
- [[file:~/.config/claude/history/sessions/2025-12/2025-12-03-151822_SESSION_claude-skills-implementation.md][Implementation Session]]
```

## Capture Workflows

### Automatic Capture (Future with Hooks)

Hooks will automatically capture:
- Session start/end events → `sessions/`
- Command executions → `execution/`
- Tool usage patterns

### Manual Capture (Current)

Create history entries for:
- **Sessions**: Significant work sessions
- **Learnings**: Problem-solving insights
- **Research**: Deep technical investigations
- **Decisions**: Architecture and design decisions

### Hybrid Approach (Recommended)

1. **During work**: Focus on the task
2. **After completion**: Create a history entry
3. **Optionally**: Create a denote note for important topics
4. **Link them**: Connect history and notes bidirectionally

## File Formats

### Session Entry
```markdown
# Session: <Description>

**Date**: YYYY-MM-DD HH:MM
**Duration**: X hours
**Context**: What prompted this work session

## What Was Accomplished
- Task 1
- Task 2
- Task 3

## Decisions Made
- Decision 1: Rationale
- Decision 2: Rationale

## Next Steps
- [ ] Follow-up task 1
- [ ] Follow-up task 2

## Related Notes
- [Note Title](~/desktop/org/notes/YYYYMMDDTHHMMSS--title__tags.org)

## Tags
#session #topic1 #topic2
```

**Note**: Use standard markdown links `[text](path)` in history entries.

### Learning Entry
```markdown
# Learning: <Description>

**Date**: YYYY-MM-DD HH:MM
**Context**: What problem was being solved

## The Problem
Clear description of the issue

## Investigation
Steps taken to understand the issue

## Solution
How it was resolved

## Key Insights
- Insight 1
- Insight 2
- Insight 3

## Prevention
How to avoid this in the future

## Related Notes
- [Note Title](~/desktop/org/notes/YYYYMMDDTHHMMSS--title__tags.org)

## Tags
#learning #bug #golang
```

### Research Entry
```markdown
# Research: <Topic>

**Date**: YYYY-MM-DD HH:MM
**Question**: What was being investigated

## Methodology
How the research was conducted

## Findings
Key discoveries and insights

## Analysis
Interpretation of findings

## Conclusions
What was learned and decided

## Sources
- Source 1
- Source 2

## Related Notes
- [Note Title](~/desktop/org/notes/YYYYMMDDTHHMMSS--title__tags.org)

## Tags
#research #topic
```

### Decision Entry
```markdown
# Decision: <Description>

**Date**: YYYY-MM-DD HH:MM
**Context**: Why this decision needed to be made

## Options Considered
1. Option A: Pros/Cons
2. Option B: Pros/Cons
3. Option C: Pros/Cons

## Decision
Which option was chosen and why

## Implications
What this means for the system

## Risks
Potential downsides and mitigations

## Related Notes
- [Note Title](~/desktop/org/notes/YYYYMMDDTHHMMSS--title__tags.org)

## Tags
#decision #architecture
```

## Denote Note Format for History

When creating a denote note for history, use the `pkai` signature to mark it as system-generated:

**Filename**:
```
YYYYMMDDTHHMMSS==pkai--description__history_category_tags.org
```

**Content**:
```org
#+title:      <Title>
#+date:       [YYYY-MM-DD Day HH:MM]
#+filetags:   :history:category:topic1:topic2:
#+identifier: YYYYMMDDTHHMMSS
#+category: pkai

* Context
Brief context of what this documents

* Details
Main content...

* Related History Entries
- [[file:~/.config/claude/history/sessions/...][Session Entry]]
- [[file:~/.config/claude/history/learnings/...][Learning Entry]]

* Related Notes
- [[file:~/desktop/org/notes/...][Other Note]]
```

**Signature**: `==pkai` stands for "Personal Knowledge Automated Infrastructure" and identifies notes created by the history system.

## Workflow Examples

### Example 1: Debugging Session

1. **During debugging**: Work on the problem
2. **After fixing**: Create learning entry
   ```bash
   # Create learning in history
   vim ~/.config/claude/history/learnings/2025-12/2025-12-03-160000_LEARNING_wireguard-connection-issue.md
   ```
3. **Optionally**: Create denote note for reference (with pkai signature)
   ```bash
   # Timestamp
   TIMESTAMP=$(date +"%Y%m%dT%H%M%S")

   # Create note with pkai signature
   vim ~/desktop/org/notes/${TIMESTAMP}==pkai--wireguard-vpn-debugging__history_learning_networking_homelab.org
   ```
4. **Link them**: Add cross-references in both files

### Example 2: Research Task

1. **Research the topic**: Gather information
2. **Create research entry**:
   ```bash
   mkdir -p ~/.config/claude/history/research/2025-12-03_nix-flake-best-practices
   vim ~/.config/claude/history/research/2025-12-03_nix-flake-best-practices/analysis.md
   ```
3. **Create denote note** for easy discovery (with pkai signature):
   ```bash
   TIMESTAMP=$(date +"%Y%m%dT%H%M%S")
   vim ~/desktop/org/notes/${TIMESTAMP}==pkai--nix-flake-best-practices-research__history_research_nix.org
   ```
4. **Link research to note** in both directions

### Example 3: Implementation Session

1. **Work on implementation**
2. **After completion**, create session entry:
   ```bash
   vim ~/.config/claude/history/sessions/2025-12/2025-12-03-150000_SESSION_claude-skills-implementation.md
   ```
3. **Update planning note** with completion status:
   ```bash
   vim ~/desktop/org/notes/20251203T151822--personal-ai-infrastructure__ai_claude_plan.org
   ```
4. **Add link from session to planning note**

## Finding History

### By Tag or Signature in Notes
```bash
# Find all history notes (by tag)
ls ~/desktop/org/notes/*__history*.org

# Find automated history notes (by pkai signature)
ls ~/desktop/org/notes/*==pkai*.org

# Find specific category
ls ~/desktop/org/notes/*__history_learning*.org
ls ~/desktop/org/notes/*==pkai*__history_session*.org
```

### By Category in History Directory
```bash
# Find all sessions
ls ~/.config/claude/history/sessions/*/*_SESSION_*.md

# Find all learnings
ls ~/.config/claude/history/learnings/*/*_LEARNING_*.md

# Find recent research
ls -lt ~/.config/claude/history/research/
```

### By Content
```bash
# Search all history
rg "wireguard" ~/.config/claude/history/

# Search history notes
rg "wireguard" ~/desktop/org/notes/*__history*.org

# Combined search
rg "wireguard" ~/.config/claude/history/ ~/desktop/org/notes/*__history*.org
```

## When to Use History vs Notes

### Use History Directory When:
- Automatic capture (via hooks)
- Raw execution logs
- Session-by-session tracking
- Chronological organization matters

### Use Denote Notes When:
- Topic-based organization
- Long-term reference material
- Linking related concepts
- Building knowledge base

### Use Both When:
- Significant learnings worth referencing
- Research worth sharing
- Important decisions
- Patterns that emerged from multiple sessions

## Benefits of Integration

1. **Chronological + Topical**: History provides timeline, notes provide topics
2. **Searchable**: Both systems are grep-friendly
3. **Discoverable**: Notes are easier to browse, history is easier to trace
4. **Flexible**: Choose the right tool for the context
5. **Connected**: Bidirectional links maintain relationships

## Best Practices

### Naming Conventions
- History files: `YYYY-MM-DD-HHMMSS_TYPE_description.md`
- History notes: `YYYYMMDDTHHMMSS--description__history_type_tags.org`

### Linking
- Always link history to related notes
- Always link notes back to history entries
- Use org-mode file links for portability

### Organization
- Keep history in `~/.config/claude/history/` (chronological)
- Keep notes in `~/desktop/org/notes/` (topical)
- Use tags consistently in both systems

### Maintenance
- Review history weekly
- Create notes for important learnings
- Archive or delete temporary work
- Keep the "good stuff" forever

## Implementation Roadmap

### Phase 1: Manual (Current)
- Manually create history entries
- Manually create denote notes
- Manual cross-linking

### Phase 2: Semi-Automated (Near Future)
- Helper commands to create entries
- Template generation
- Automatic cross-linking

### Phase 3: Fully Automated (Future)
- Hooks capture sessions automatically
- AI suggests what to save
- Automatic note generation for significant work

## Quick Reference

### Create Session Entry
```bash
DATE=$(date +"%Y-%m")
TIMESTAMP=$(date +"%Y-%m-%d-%H%M%S")
mkdir -p ~/.config/claude/history/sessions/$DATE
vim ~/.config/claude/history/sessions/$DATE/${TIMESTAMP}_SESSION_description.md
```

### Create Learning Entry
```bash
DATE=$(date +"%Y-%m")
TIMESTAMP=$(date +"%Y-%m-%d-%H%M%S")
mkdir -p ~/.config/claude/history/learnings/$DATE
vim ~/.config/claude/history/learnings/$DATE/${TIMESTAMP}_LEARNING_description.md
```

### Create History Note (with pkai signature)
```bash
TIMESTAMP=$(date +"%Y%m%dT%H%M%S")
vim ~/desktop/org/notes/${TIMESTAMP}==pkai--topic__history_category_tags.org
```

Remember: **When in doubt, save to history!** It's better to capture too much than to lose valuable insights.
