---
name: Git
description: Git version control best practices and workflows. USE WHEN working with git commands, commits, branches, pull requests, rebasing, or version control operations.
---

# Git Best Practices

## Purpose
Guide git usage following best practices, conventional commits, and project-specific workflows.

## Commit Message Philosophy

**Focus on WHY, not WHAT**

The code diff shows WHAT changed. Your commit message should explain WHY it changed and WHAT IMPACT it has.

### Good Example
```
feat: Add user authentication system

- Enable secure user sessions for multi-user support
- Prevent unauthorized API access with JWT validation
- Establish foundation for role-based permissions
```

### Bad Example
```
feat: Add auth

- Added login.ts file
- Added middleware
- Added tests
```

## Commit Message Format

### Structure

```
<type>: <short description (max 80 chars)>

- <bullet point describing impact/reason>
- <bullet point describing impact/reason>
- <bullet point describing impact/reason>

Signed-off-by: Vincent Demeester <vincent@sbr.pm>
Co-Authored-By: Claude <noreply@anthropic.com>
```

### Commit Types

- **feat**: New feature or capability
- **fix**: Bug fix
- **chore**: Maintenance, dependencies, tooling
- **docs**: Documentation only
- **refactor**: Code restructuring without behavior change
- **test**: Adding or updating tests
- **perf**: Performance improvement
- **style**: Code style/formatting (not visual style)
- **ci**: CI/CD pipeline changes

### Guidelines

**First Line**:
- Clear, concise summary under 80 characters
- Use imperative mood ("Add", "Fix", "Update", not "Added", "Fixed")
- Start with commit type

**Bullet Points**:
- 2-3 bullets maximum (not more)
- Each bullet under 80 characters
- Focus on WHY and IMPACT, not WHAT
- Explain the reasoning and consequences
- Think: "What problem does this solve?"

## Mandatory Commit Requirements

### Always Include

1. **--signoff**: ALWAYS use the `--signoff` flag
   ```bash
   git commit --signoff -m "message"
   ```
   This adds: `Signed-off-by: Vincent Demeester <vincent@sbr.pm>`

2. **Co-Authored-By**: When working with Claude Code
   ```
   Co-Authored-By: Claude <noreply@anthropic.com>
   ```

### Never Include

**❌ NO EMOJIS**: Never use 🚀, ✨, 🎉, or any emojis in commit messages
- Keep commits professional
- Emojis don't add value and can cause encoding issues
- Focus on clear, descriptive text

## Commit Best Practices

### 1. Atomic Commits
- One logical change per commit
- Commits should be self-contained and reversible
- If you can't describe it in one sentence, it's too big

### 2. Review Before Committing
```bash
# Always review what's staged
git diff --staged

# Check each file individually
git diff --staged src/auth/login.ts

# Verify no secrets
grep -r "API_KEY\|SECRET\|PASSWORD" .
```

### 3. One Concern Per Commit

**❌ Bad**: Multiple unrelated changes
```bash
git add src/auth/ src/ui/ docs/
git commit -m "Updates"
```

**✅ Good**: Focused, logical commits
```bash
git add src/auth/
git commit --signoff -m "feat: Add JWT authentication"

git add src/ui/
git commit --signoff -m "feat: Add login form component"

git add docs/
git commit --signoff -m "docs: Document authentication flow"
```

## Commit Message Examples

### Feature
```
feat: Implement user authentication

- Enable secure multi-user sessions with JWT
- Prevent unauthorized access to protected routes
- Lay groundwork for role-based access control

Signed-off-by: Vincent Demeester <vincent@sbr.pm>
Co-Authored-By: Claude <noreply@anthropic.com>
```

### Bug Fix
```
fix: Prevent null pointer in user validation

- Resolve crashes when user object is undefined
- Ensure defensive checks at API boundaries

Signed-off-by: Vincent Demeester <vincent@sbr.pm>
Co-Authored-By: Claude <noreply@anthropic.com>
```

### Refactoring
```
refactor: Simplify error handling in API layer

- Reduce code duplication across endpoints
- Improve error message clarity for debugging
- Make error handling more maintainable

Signed-off-by: Vincent Demeester <vincent@sbr.pm>
Co-Authored-By: Claude <noreply@anthropic.com>
```

### Chore
```
chore: Update dependencies and resolve warnings

- Address security vulnerabilities in npm packages
- Fix ESLint warnings for cleaner codebase

Signed-off-by: Vincent Demeester <vincent@sbr.pm>
Co-Authored-By: Claude <noreply@anthropic.com>
```

## Creating Commits with HEREDOC

To ensure proper formatting with multi-line messages:

```bash
git commit --signoff --message "$(cat <<'EOF'
feat: Add user authentication system

- Enable secure user sessions for multi-user support
- Prevent unauthorized API access with JWT validation
- Establish foundation for role-based permissions

Co-Authored-By: Claude <noreply@anthropic.com>
EOF
)"
```

**Why HEREDOC?**
- Ensures proper multi-line formatting
- Prevents shell escaping issues
- Preserves formatting exactly as written

## Branch Management

### Naming Conventions

```
feature/<description>  # New features
fix/<description>      # Bug fixes
chore/<description>    # Maintenance
docs/<description>     # Documentation
refactor/<description> # Refactoring
```

Examples:
- `feature/user-authentication`
- `fix/null-pointer-validation`
- `chore/update-dependencies`

### Branch Workflow

```bash
# Create feature branch
git checkout -b feature/user-auth

# Make changes and commit
git add src/auth/
git commit --signoff -m "feat: Add JWT authentication"

# Keep updated with main (rebase, don't merge)
git fetch origin
git rebase origin/main

# Push to remote
git push -u origin feature/user-auth
```

## Amending Commits

### When to Amend

**✅ Safe to amend**:
- Last commit
- Not yet pushed
- You are the author

**❌ Never amend**:
- Commits already pushed to shared branches
- Commits authored by someone else
- Commits on main/master

### How to Amend

```bash
# Check authorship first
git log -1 --format='%an %ae'

# Verify not pushed
git status  # Should show "Your branch is ahead"

# Amend if both conditions true
git add forgotten-file.ts
git commit --amend --no-edit

# Or change message
git commit --amend
```

## Rebasing

### Interactive Rebase

```bash
# Clean up last 3 commits
git rebase -i HEAD~3

# In editor:
# pick   - keep commit as-is
# reword - change commit message
# squash - merge into previous commit (keep message)
# fixup  - merge into previous commit (discard message)
# drop   - remove commit
```

### Rebase on Main

```bash
# Update feature branch with latest main
git fetch origin
git rebase origin/main

# If conflicts occur
git status              # See conflicting files
# Edit files to resolve
git add <resolved-files>
git rebase --continue

# Or abort if needed
git rebase --abort
```

## Stashing

```bash
# Save work in progress
git stash push -m "WIP: refactoring auth"

# List stashes
git stash list

# Apply and remove
git stash pop

# Apply without removing
git stash apply stash@{0}

# Drop a stash
git stash drop stash@{0}
```

## Pull Requests

### Creating PRs

```bash
# Ensure branch is up to date
git fetch origin
git rebase origin/main

# Push to remote
git push -u origin feature/my-feature

# Create PR (using gh CLI)
gh pr create --title "feat: Add user authentication" --body "$(cat <<'EOF'
## Summary
- Enable secure user sessions for multi-user support
- Prevent unauthorized API access with JWT validation
- Establish foundation for role-based permissions

## Test plan
- [ ] Test user login flow
- [ ] Test token validation
- [ ] Test unauthorized access prevention
EOF
)"
```

### PR Best Practices

- **One PR = One Feature**: Keep PRs focused
- **Small PRs**: Easier to review, faster to merge
- **Clean commits**: Squash fixup commits before merging
- **Update regularly**: Rebase on main frequently
- **Respond to reviews**: Address feedback promptly

## Pre-commit Hooks

The home repository uses pre-commit hooks for:
- **nixfmt-rfc-style**: Nix formatting
- **deadnix**: Remove dead Nix code
- **gofmt**: Go formatting
- **shellcheck**: Shell script linting
- **flake8, ruff**: Python linting

### If Hooks Fail

```bash
# Format files
make fmt

# Re-add specific formatted files and commit
git add path/to/formatted/file.nix
git commit --signoff -m "your message"
```

## Common Workflows

### Feature Development

```bash
# 1. Create branch
git checkout -b feature/new-feature

# 2. Make changes and commit atomically
git add src/module1/
git commit --signoff -m "feat: Add module1 functionality"

git add src/module2/
git commit --signoff -m "feat: Add module2 integration"

# 3. Keep updated
git fetch origin
git rebase origin/main

# 4. Push
git push -u origin feature/new-feature

# 5. Create PR
gh pr create
```

### Fixing a Bug

```bash
# 1. Create fix branch
git checkout -b fix/null-pointer-crash

# 2. Commit the fix
git add src/validation.ts
git commit --signoff -m "fix: Prevent null pointer in user validation

- Resolve crashes when user object is undefined
- Ensure defensive checks at API boundaries
"

# 3. Push and create PR
git push -u origin fix/null-pointer-crash
gh pr create
```

## Troubleshooting

### Undo Last Commit (Keep Changes)
```bash
git reset --soft HEAD~1
```

### Undo Last Commit (Discard Changes)
```bash
git reset --hard HEAD~1
```

### Fix Wrong Branch
```bash
# Committed to main instead of feature branch
git branch feature/new-work
git reset --hard origin/main
git checkout feature/new-work
```

### Recover Deleted Commit
```bash
# Find lost commit
git reflog

# Restore it
git checkout <commit-hash>
git checkout -b recovered-branch
```

### Remove File from Staging
```bash
git reset HEAD file.txt
```

## Git Configuration

### Required Settings

```bash
# User identity (required for --signoff)
git config --global user.name "Vincent Demeester"
git config --global user.email "vincent@sbr.pm"
```

### Recommended Settings

```bash
# Default branch name
git config --global init.defaultBranch main

# Rebase by default when pulling
git config --global pull.rebase true

# Prune on fetch
git config --global fetch.prune true

# Better diff output
git config --global diff.algorithm histogram

# Show original in merge conflicts
git config --global merge.conflictstyle diff3
```

## Preferred Method: Use /git-commit:commit Command

**IMPORTANT**: When creating commits, ALWAYS prefer using the `/git-commit:commit` slash command instead of running `git commit` directly.

The `/git-commit:commit` slash command provides a guided workflow:

1. **Parallel information gathering** - Fast git status, diff, log
2. **File-by-file review** - Understand each change
3. **Commit message creation** - Focus on WHY and impact
4. **User approval** - Explicit confirmation before committing
5. **Smart push offer** - Automatic push handling
6. **Explicit file staging** - Never uses `git add -A` or `git add .`

The command follows all the best practices in this skill and ensures:
- Proper `--signoff` usage
- `Co-Authored-By: Claude` attribution
- No emojis
- Focus on WHY over WHAT
- Atomic, well-structured commits
- Explicit file staging (never adds all files blindly)

**When to use it**:
- Creating any new commit
- Committing staged or unstaged changes
- Need to review changes before committing

**When you can use git directly**:
- Amending the last commit (`git commit --amend`)
- Specific git operations like rebase, cherry-pick, etc.

## Anti-Patterns to Avoid

### ❌ Vague Messages
```bash
git commit -m "Update code"
git commit -m "Fix stuff"
git commit -m "WIP"
```

### ❌ Including Emojis
```bash
git commit -m "✨ Add new feature"  # NO!
git commit -m "🚀 Deploy changes"   # NO!
```

### ❌ Describing WHAT Instead of WHY
```bash
git commit -m "feat: Add login function

- Created login.ts
- Added handleLogin method
- Imported jwt library
"
```

### ❌ Force Pushing to Shared Branches
```bash
git push --force origin main  # NEVER!
```

### ❌ Using git add -A or git add .
```bash
git add -A   # NEVER! Too broad, adds everything
git add .    # NEVER! Too broad, adds everything
```

**Why to avoid**:
- Adds ALL changes including unintended files
- Can include generated files (build outputs, dependencies)
- Can include sensitive files (.env, credentials)
- Bypasses intentional staging control
- Makes commits less atomic and focused

**✅ Instead, explicitly add files or directories**:
```bash
git add src/auth/
git add docs/authentication.md
git add tests/auth.test.ts
```

### ❌ Committing Secrets
```bash
git add .env  # Check for secrets first!
git add config/credentials.json  # NO!
```

## Summary

**Key Takeaways**:

1. ✅ **ALWAYS use `--signoff`**
2. ✅ **Include `Co-Authored-By: Claude`** when working with AI
3. ❌ **NEVER use emojis** in commit messages
4. ❌ **NEVER use `git add -A` or `git add .`** - always add files explicitly
5. ✅ **Focus on WHY and IMPACT**, not WHAT
6. ✅ **Keep commits atomic** - one logical change per commit
7. ✅ **Use HEREDOC** for multi-line commit messages
8. ✅ **Rebase, don't merge** when updating feature branches
9. ✅ **Review before committing** - check diffs and verify no secrets
10. ✅ **2-3 bullet points** maximum, each under 80 chars
11. ✅ **Test before committing** - run pre-commit checks

**Remember**: Future you will thank present you for clear, descriptive commit messages that explain WHY changes were made!
