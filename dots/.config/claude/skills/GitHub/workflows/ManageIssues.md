# ManageIssues Workflow

Create, list, view, and manage GitHub issues.

## When to Use

- "create issue"
- "list issues"
- "view issue"
- "close issue"
- "update issue"

## Quick Commands

### List Issues

```bash
# List open issues
gh issue list

# List all issues (including closed)
gh issue list --state all

# Filter by label
gh issue list --label bug

# Filter by assignee
gh issue list --assignee @me

# Search issues
gh issue list --search "authentication"
```

### View Issue

```bash
# View issue details
gh issue view <number>

# View in browser
gh issue view <number> --web

# View comments
gh issue view <number> --comments
```

### Create Issue

```bash
# Interactive
gh issue create

# With title and body
gh issue create --title "Bug: Login fails" --body "Description here"

# With labels and assignee
gh issue create --title "Bug" --label bug --assignee @me

# From template
gh issue create --template bug_report.md
```

### Update Issue

```bash
# Close issue
gh issue close <number>

# Reopen issue
gh issue reopen <number>

# Edit issue
gh issue edit <number> --title "New title" --body "New body"

# Add labels
gh issue edit <number> --add-label bug,priority:high

# Assign
gh issue edit <number> --add-assignee @user
```

## Workflow Steps

### 1. Create Issue from Problem

When you encounter a bug or have a feature request:

```bash
gh issue create --title "feat: Add dark mode support" --body "$(cat <<'EOF'
## Feature Request

Add dark mode support to the application.

## Motivation
Users working in low-light environments would benefit from a dark theme.

## Proposed Solution
- Add theme toggle in settings
- Support system preference detection
- Persist user preference

## Alternatives Considered
- Browser extension (not ideal for all users)
- Manual CSS overrides (poor UX)

## Additional Context
Similar to #123 but for the entire UI.
EOF
)"
```

### 2. Track and Organize

```bash
# Add to project
gh issue edit <number> --add-project "Q1 2025"

# Set milestone
gh issue edit <number> --milestone "v2.0"

# Add labels
gh issue edit <number> --add-label enhancement,ui,needs-design
```

### 3. Link to PRs

```bash
# In PR description, use:
Closes #<issue-number>
Fixes #<issue-number>
Resolves #<issue-number>
```

## Common Scenarios

### Scenario 1: Bug Report

```bash
gh issue create \
  --title "bug: Login redirect fails on mobile" \
  --label bug,mobile \
  --body "$(cat <<'EOF'
## Bug Description
Login redirect doesn't work on mobile Safari

## Steps to Reproduce
1. Open site on iPhone Safari
2. Click login button
3. Enter credentials
4. Observe redirect fails

## Expected Behavior
Should redirect to dashboard

## Actual Behavior
Stays on login page, shows error in console

## Environment
- OS: iOS 17.2
- Browser: Safari
- Device: iPhone 14

## Additional Context
Works fine on desktop Chrome
EOF
)"
```

### Scenario 2: Feature Request

```bash
gh issue create \
  --title "feat: Add export to CSV functionality" \
  --label enhancement,data-export \
  --body "## Feature Request

Allow users to export data to CSV format.

## Use Case
Users need to analyze data in Excel/Google Sheets

## Acceptance Criteria
- [ ] Export button in data table
- [ ] Includes all visible columns
- [ ] Respects current filters
- [ ] Handles large datasets (>10k rows)

## Priority
Medium - requested by 5 customers"
```

### Scenario 3: Find and Fix

```bash
# Find bugs assigned to me
gh issue list --assignee @me --label bug

# Pick one to work on
gh issue view 42

# Create branch
git checkout -b fix/issue-42-login-redirect

# Make changes, commit, create PR
gh pr create --fill

# PR body includes "Closes #42"
```

## Best Practices

1. **Use templates**: Create issue templates for consistency
2. **Clear titles**: Describe the issue, not the solution
3. **Provide context**: Include steps to reproduce for bugs
4. **Label appropriately**: Use labels for categorization
5. **Assign ownership**: Assign issues to track responsibility
6. **Link PRs**: Always link PRs that fix issues
7. **Close when done**: Don't leave resolved issues open
8. **Update status**: Comment on progress for long-running issues

## Resources

- [gh issue documentation](https://cli.github.com/manual/gh_issue)
- [GitHub issues guide](https://docs.github.com/en/issues)
