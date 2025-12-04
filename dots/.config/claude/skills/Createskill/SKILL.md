---
name: Createskill
description: Skill creation framework for creating, validating, updating, or canonicalizing skills. USE WHEN user wants to create a new skill, validate skill structure, update existing skill, or fix skill compliance.
---

# Createskill

Systematic skill creation framework for building consistent, well-structured Claude Code skills.

## TitleCase Naming Convention

**All naming must use TitleCase (PascalCase).**

| Component | Format | Example |
|-----------|--------|---------|
| Skill directory | TitleCase | `Golang`, `Homelab`, `Createskill` |
| Workflow files | TitleCase.md | `Create.md`, `UpdateInfo.md` |
| Reference docs | TitleCase.md | `Guide.md`, `Reference.md` |
| Tool files | TitleCase.ts | `ManageTool.ts` |

**Wrong (NEVER use):**
- `createskill`, `create-skill`, `CREATE_SKILL`
- `create.md`, `update-info.md`, `SYNC_REPO.md`

## Workflow Routing

**When executing a workflow, output this notification directly:**

```
Running the **WorkflowName** workflow from the **Createskill** skill...
```

| Workflow | Trigger | File |
|----------|---------|------|
| **CreateSkill** | "create a new skill" | `workflows/CreateSkill.md` |
| **ValidateSkill** | "validate skill", "check skill" | `workflows/ValidateSkill.md` |

## Skill Structure Requirements

### Required Components

Every skill must have:

1. **SKILL.md** - Main skill file with:
   - YAML frontmatter (name, description)
   - Clear purpose and scope
   - Workflow routing table
   - Usage examples

2. **workflows/** directory - Contains workflow markdown files

3. **tools/** directory - Contains tool scripts (optional)

### YAML Frontmatter Format

```yaml
---
name: SkillName
description: Single-line description. USE WHEN triggers go here.
---
```

**Critical Rules:**
- `name` must be TitleCase
- `description` must be ONE LINE (no newlines)
- Include "USE WHEN" triggers in description

### Markdown Body Structure

Required sections in order:

1. **# [Skill Name]** - H1 heading matching the name
2. **Brief overview** - What this skill does
3. **Workflow Routing** - Table of workflows
4. **Examples** - Usage examples (at least one)

## Examples

**Example 1: Create a new skill from scratch**
```
User: "Create a skill for managing my Docker containers"
→ Invokes CreateSkill workflow
→ Creates skill directory with TitleCase naming
→ Creates SKILL.md with proper frontmatter
→ Creates workflows/ and tools/ directories
→ Generates USE WHEN triggers based on purpose
```

**Example 2: Validate an existing skill**
```
User: "Validate the golang skill"
→ Invokes ValidateSkill workflow
→ Checks SKILL.md structure and naming
→ Verifies TitleCase naming throughout
→ Verifies USE WHEN triggers are clear
→ Reports any compliance issues
```

## Best Practices

### Naming
- Use TitleCase for all directories and files
- Keep names descriptive but concise
- Match skill directory name to YAML `name` field

### Description Field
- Keep it to ONE LINE only
- Include clear USE WHEN triggers
- Focus on when to invoke the skill, not just what it does

### Workflows
- Name workflows with clear action verbs (Create, Update, Validate)
- Keep workflows focused on single tasks
- Use TitleCase.md naming

### Examples
- Provide at least one concrete example
- Show the trigger phrase and expected behavior
- Demonstrate the value of the skill

## Integration

This skill works with your existing skills:
- References `.config/claude/skills/` directory structure
- Follows patterns from CORE, golang, nix, homelab, notes skills
- Validates against established conventions
