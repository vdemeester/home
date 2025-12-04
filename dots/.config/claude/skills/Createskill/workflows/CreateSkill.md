# CreateSkill Workflow

Create a new skill from scratch with proper structure and naming conventions.

## Steps

### 1. Understand the Purpose

Ask the user:
- What is this skill for?
- What workflows or tasks should it include?
- When should this skill be invoked?

### 2. Choose the Name

- Use TitleCase (PascalCase)
- Examples: `Docker`, `Kubernetes`, `WebDev`, `DataAnalysis`
- Never use: `docker`, `web-dev`, `data_analysis`

### 3. Create Directory Structure

```bash
mkdir -p ~/.config/claude/skills/[SkillName]/{workflows,tools}
```

### 4. Create SKILL.md

Create `~/.config/claude/skills/[SkillName]/SKILL.md` with:

```yaml
---
name: SkillName
description: Brief description. USE WHEN user mentions [trigger1], [trigger2], or needs [specific capability].
---

# SkillName

Brief overview of what this skill provides.

## Workflow Routing

| Workflow | Trigger | File |
|----------|---------|------|
| **WorkflowName** | "trigger phrase" | `workflows/WorkflowName.md` |

## Examples

**Example 1: [Use case]**
```
User: "[trigger phrase]"
→ Invokes [WorkflowName] workflow
→ [What it does]
```
```

### 5. Create Initial Workflow

If the user wants workflows, create them in `workflows/` directory using TitleCase naming.

### 6. Verify Structure

Check:
- ✓ Directory name is TitleCase
- ✓ SKILL.md has valid YAML frontmatter
- ✓ Description is ONE LINE with USE WHEN triggers
- ✓ Workflow files use TitleCase.md naming
- ✓ At least one example is provided

### 7. Present to User

Show the created structure and ask for confirmation or adjustments.

## Output Format

```
Created skill: [SkillName]

Structure:
~/.config/claude/skills/[SkillName]/
├── SKILL.md
├── workflows/
│   └── [WorkflowName].md
└── tools/

Files created:
- SKILL.md: Main skill definition
- workflows/[WorkflowName].md: [Description]

Next steps:
- Add more workflows as needed
- Test the skill by invoking it
- Refine USE WHEN triggers based on usage
```
