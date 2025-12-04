# ValidateSkill Workflow

Validate a skill's structure, naming, and compliance with conventions.

## Steps

### 1. Read the Skill File

Read `~/.claude/skills/[SkillName]/SKILL.md`

### 2. Check Directory Naming

- ✓ Directory name is TitleCase (e.g., `Golang`, not `golang`)
- ✓ Directory name matches YAML `name` field

### 3. Validate YAML Frontmatter

Required fields:
```yaml
---
name: SkillName          # Must be TitleCase
description: ...         # Must be ONE LINE with USE WHEN triggers
---
```

Check:
- ✓ `name` field exists and is TitleCase
- ✓ `description` field exists and is single line
- ✓ Description includes "USE WHEN" or clear triggers
- ✓ No syntax errors in YAML

### 4. Validate Markdown Body

Required structure:
1. ✓ H1 heading matching the skill name
2. ✓ Brief overview paragraph
3. ✓ "Workflow Routing" section (if workflows exist)
4. ✓ "Examples" section with at least one example

### 5. Check Workflow Files

For each file in `workflows/`:
- ✓ Uses TitleCase.md naming (e.g., `Create.md`, not `create.md`)
- ✓ Listed in Workflow Routing table
- ✓ Has clear structure

### 6. Check Tool Files

For each file in `tools/`:
- ✓ Uses TitleCase naming (e.g., `GenerateTool.ts`)
- ✓ Is executable (if shell script)

### 7. Generate Compliance Report

## Output Format

```
Validating skill: [SkillName]

✓ Directory Structure
  ✓ Name is TitleCase: [SkillName]
  ✓ workflows/ directory exists
  ✓ tools/ directory exists

✓ SKILL.md Frontmatter
  ✓ name: [SkillName] (TitleCase)
  ✓ description: Single line with triggers

✓ SKILL.md Body
  ✓ H1 heading matches name
  ✓ Has overview section
  ✓ Has Workflow Routing table
  ✓ Has Examples section

✓ Workflows
  ✓ All workflows use TitleCase.md
  ✓ All listed in routing table

Compliance: PASSED

[If issues found:]
❌ Issues Found:
  - [Issue 1 description]
  - [Issue 2 description]

Recommendations:
  - [How to fix issue 1]
  - [How to fix issue 2]
```

## Failure Handling

If validation fails, suggest fixes:
- Naming issues: Provide correct TitleCase names
- Structure issues: Show required structure
- Missing sections: Provide templates
