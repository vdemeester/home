# CreatePR Workflow

Create a GitHub pull request with proper formatting, labels, and metadata.

## When to Use

- "create pr"
- "open pull request"
- "new pr"
- "create pull request"

## Quick Commands

```bash
# Interactive PR creation
gh pr create

# With title and body
gh pr create --title "feat: Add user authentication" --body "Description here"

# From template
gh pr create --template feature.md

# Set base branch
gh pr create --base main

# Add reviewers, assignees, labels
gh pr create --reviewer @user1,@user2 --assignee @me --label enhancement

# Create as draft
gh pr create --draft

# Fill from commit messages
gh pr create --fill
```

## Workflow Steps

### 1. Verify Current State

```bash
# Check current branch
git branch --show-current

# Ensure changes are committed
git status

# Check if PR already exists
gh pr status
```

### 2. Check for PR Template

**Look for templates in common locations:**

```bash
# Check for pull request templates
TEMPLATE=""

# Check common locations
if [ -f .github/pull_request_template.md ]; then
  TEMPLATE=".github/pull_request_template.md"
elif [ -f .github/PULL_REQUEST_TEMPLATE.md ]; then
  TEMPLATE=".github/PULL_REQUEST_TEMPLATE.md"
elif [ -f docs/pull_request_template.md ]; then
  TEMPLATE="docs/pull_request_template.md"
elif [ -f PULL_REQUEST_TEMPLATE.md ]; then
  TEMPLATE="PULL_REQUEST_TEMPLATE.md"
elif [ -d .github/PULL_REQUEST_TEMPLATE ]; then
  # Multiple templates, list them
  ls .github/PULL_REQUEST_TEMPLATE/
fi

if [ -n "$TEMPLATE" ]; then
  echo "Found PR template: $TEMPLATE"
  cat "$TEMPLATE"
fi
```

**Common template locations:**
- `.github/pull_request_template.md` (default)
- `.github/PULL_REQUEST_TEMPLATE.md` (uppercase)
- `.github/PULL_REQUEST_TEMPLATE/feature.md` (multiple templates)
- `docs/pull_request_template.md`
- `PULL_REQUEST_TEMPLATE.md` (root)

### 3. Generate PR Content Following Template

**If template exists, follow its structure:**

Example template:
```markdown
## Description
<!-- Describe your changes -->

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Breaking change
- [ ] Documentation update

## Checklist
- [ ] Tests added/updated
- [ ] Documentation updated
- [ ] All checks passing
```

**Fill in based on commits:**

```bash
# Read template
TEMPLATE_CONTENT=$(cat .github/pull_request_template.md)

# Generate filled content
cat > /tmp/pr_body.md <<EOF
## Description
$(git log main..HEAD --format=%B | head -5)

## Type of Change
- [x] New feature
- [ ] Bug fix
- [ ] Breaking change
- [ ] Documentation update

## Checklist
- [x] Tests added/updated
- [x] Documentation updated
- [ ] All checks passing

## Related Issues
Closes #42
EOF
```

**If no template, use default format:**

```markdown
## Summary
Brief overview of changes

## Changes
- Change 1
- Change 2
- Change 3

## Testing
- [ ] Unit tests added/updated
- [ ] Manual testing completed
- [ ] All checks passing

## Related Issues
Closes #123
Relates to #456
```

### 4. Extract Title and Body

**Title format** (conventional commits):
```
<type>: <short description>

Types: feat, fix, docs, chore, refactor, test, perf, ci
```

**Generate from commits:**
```bash
# Get commit messages since diverging from main
git log main..HEAD --oneline

# Generate title from first commit
TITLE=$(git log main..HEAD --format=%s | head -1)

# Or use conventional commit format
TITLE="feat: Add user authentication"
```

### 5. Create PR with Template Content

**Using template file directly:**
```bash
# gh pr create will automatically use template if it exists
gh pr create --fill
```

**Using custom template:**
```bash
# Specify template
gh pr create --template feature.md
```

**Manual creation with template content:**
```bash
# Read and fill template
BODY=$(cat .github/pull_request_template.md)

# Replace placeholders or fill sections
BODY=$(echo "$BODY" | sed 's/<!-- Describe your changes -->/Implements JWT authentication/')

gh pr create \
  --title "feat: Add user authentication" \
  --body "$BODY" \
  --base main
```

### 6. Verify and Monitor

```bash
# View created PR
gh pr view

# Check status
gh pr checks --watch

# Get PR URL
gh pr view --json url --jq '.url'
```

## Template Detection and Usage

### Automatic Detection

The `gh pr create` command automatically detects and uses templates:

```bash
# Will use template if found
gh pr create

# Will prompt for which template if multiple exist
gh pr create
```

### Manual Template Selection

```bash
# List available templates
ls .github/PULL_REQUEST_TEMPLATE/

# Use specific template
gh pr create --template bug_fix.md
gh pr create --template feature.md
gh pr create --template hotfix.md
```

### Parse and Fill Template

For intelligent template filling:

```bash
#!/bin/bash
# parse_template.sh

TEMPLATE=".github/pull_request_template.md"

if [ ! -f "$TEMPLATE" ]; then
  echo "No template found"
  exit 1
fi

# Read template
CONTENT=$(cat "$TEMPLATE")

# Extract sections and fill
# Example: Fill "Type of Change" based on branch name
BRANCH=$(git branch --show-current)

if [[ $BRANCH == feat/* ]]; then
  CONTENT=$(echo "$CONTENT" | sed 's/\[ \] New feature/[x] New feature/')
elif [[ $BRANCH == fix/* ]]; then
  CONTENT=$(echo "$CONTENT" | sed 's/\[ \] Bug fix/[x] Bug fix/')
fi

# Auto-check completed items
# If tests exist, mark checkbox
if git diff main..HEAD --name-only | grep -q "test"; then
  CONTENT=$(echo "$CONTENT" | sed '0,/\[ \] Tests added/s/\[ \] Tests added/[x] Tests added/')
fi

echo "$CONTENT"
```

## Advanced Usage

### Create Custom Template

Create `.github/pull_request_template.md`:
```markdown
## Description
<!-- Describe your changes -->

## Type of Change
- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] Documentation update
- [ ] Code refactoring
- [ ] Performance improvement
- [ ] Test addition/update

## Motivation and Context
<!-- Why is this change required? What problem does it solve? -->
<!-- If it fixes an open issue, please link to the issue here -->

## How Has This Been Tested?
<!-- Describe the tests you ran to verify your changes -->
- [ ] Unit tests
- [ ] Integration tests
- [ ] Manual testing

## Screenshots (if appropriate)
<!-- Add screenshots to help explain your changes -->

## Checklist
- [ ] My code follows the code style of this project
- [ ] I have updated the documentation accordingly
- [ ] I have added tests to cover my changes
- [ ] All new and existing tests passed
- [ ] My changes generate no new warnings
- [ ] I have checked my code and corrected any misspellings

## Related Issues
<!-- Link related issues: Closes #123, Relates to #456 -->
```

### Multiple Templates

Create directory `.github/PULL_REQUEST_TEMPLATE/`:

```bash
.github/PULL_REQUEST_TEMPLATE/
├── bug_fix.md
├── feature.md
├── hotfix.md
└── documentation.md
```

Use specific template:
```bash
gh pr create --template bug_fix.md
```

### Auto-Fill PR

```bash
# Fill title from first commit, body from rest
gh pr create --fill

# Fill with verbose output
gh pr create --fill-verbose
```

### Draft PR

For work in progress:
```bash
# Create as draft
gh pr create --draft

# Mark ready later
gh pr ready <number>
```

## Common Scenarios

### Scenario 1: Quick PR (with template)

```bash
# gh automatically detects and uses template
gh pr create --fill --web
```

### Scenario 2: Custom Template Filling

```bash
# Detect template
TEMPLATE=".github/pull_request_template.md"

# Fill template intelligently
BODY=$(cat "$TEMPLATE")

# Auto-check items based on changes
if git diff main..HEAD --name-only | grep -q "_test\|spec"; then
  BODY=$(echo "$BODY" | sed '0,/\[ \] Tests added/s/\[ \] Tests added/[x] Tests added/')
fi

if git diff main..HEAD --name-only | grep -q "README\|docs/"; then
  BODY=$(echo "$BODY" | sed '0,/\[ \] Documentation updated/s/\[ \] Documentation updated/[x] Documentation updated/')
fi

gh pr create \
  --title "feat: Add user authentication" \
  --body "$BODY" \
  --reviewer @team
```

### Scenario 3: Cross-Fork PR

```bash
# From your fork to upstream
gh pr create \
  --repo upstream/repo \
  --base main \
  --head yourname:feature-branch
```

## Custom Tool: Smart PR Creation

For intelligent template filling and validation:

```bash
# In tools/ directory
./CreatePR.ts --auto-fill --validate-checklist --detect-type
```

**Tool capabilities:**
- Auto-detect PR template location
- Intelligently fill template sections based on commits
- Validate all checklist items are addressed
- Auto-detect PR type from branch name or commits
- Link related issues automatically
- Suggest reviewers based on file changes
- Validate PR against project guidelines

## Best Practices

1. **Use repository template**: Always use the repo's PR template if it exists
2. **Fill template completely**: Don't leave sections empty or with placeholders
3. **Check all boxes**: Actually verify checklist items before marking
4. **Descriptive titles**: Follow conventional commit format
5. **Comprehensive description**: Explain WHY, not just WHAT
6. **Link issues**: Use "Closes #123" to auto-close
7. **Add reviewers**: Don't leave empty, assign relevant people
8. **Use labels**: Categorize for better tracking
9. **Include tests**: Mention test coverage in description
10. **Check before creating**: Ensure commits are ready
11. **Use draft for WIP**: Don't request review for incomplete work

## Template Best Practices

1. **Keep templates concise**: Don't overwhelm contributors
2. **Use clear sections**: Organize with headers
3. **Provide examples**: Show what good looks like
4. **Make checklists actionable**: Specific, verifiable items
5. **Link to contributing guide**: Reference detailed guidelines
6. **Support multiple types**: Create separate templates for bugs/features
7. **Include issue links**: Remind to link related issues

## Resources

- [gh pr create documentation](https://cli.github.com/manual/gh_pr_create)
- [Pull request templates](https://docs.github.com/en/communities/using-templates-to-encourage-useful-issues-and-pull-requests/creating-a-pull-request-template-for-your-repository)
- [Pull request best practices](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests)
