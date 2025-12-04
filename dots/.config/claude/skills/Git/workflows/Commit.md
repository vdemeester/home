# Commit Workflow

Create well-structured, meaningful commits following conventional commit standards.

## When to Use

- "create git commit"
- "commit changes"
- "write commit message"
- "amend commit"

## Quick Commands

### Basic Commits
```bash
# Stage and commit
git add file.txt
git commit -m "feat: add new feature"

# Commit with signoff
git commit --signoff -m "fix: resolve bug"

# Commit all tracked changes
git commit -am "chore: update dependencies"

# Use HEREDOC for multi-line messages
git commit --signoff --message "$(cat <<'EOF'
feat: add user authentication

- Enable secure sessions with JWT
- Add login/logout endpoints
- Implement password hashing
EOF
)"
```

### Viewing Changes Before Commit
```bash
# See what will be committed
git diff --staged

# Review each file
git diff --staged file.txt

# Check commit status
git status
```

## Commit Message Format

### Conventional Commits Structure
```
<type>: <short description (max 72 chars)>

- <bullet point describing WHY and IMPACT>
- <bullet point describing WHY and IMPACT>
- <bullet point describing WHY and IMPACT>

Signed-off-by: Your Name <your@email.com>
```

### Commit Types
- **feat**: New feature
- **fix**: Bug fix
- **docs**: Documentation only
- **style**: Code style (formatting, no logic change)
- **refactor**: Code restructure (no behavior change)
- **perf**: Performance improvement
- **test**: Adding/updating tests
- **chore**: Maintenance, dependencies, tooling
- **ci**: CI/CD changes

### Good vs Bad Examples

**Good:**
```
feat: add user authentication system

- Enable secure multi-user sessions with JWT tokens
- Prevent unauthorized API access
- Lay groundwork for role-based permissions

Signed-off-by: Vincent Demeester <vincent@sbr.pm>
```

**Bad:**
```
added auth stuff

- added login.ts
- updated config
```

## Staging Strategies

### Selective Staging
```bash
# Stage specific files
git add src/auth.ts src/types.ts

# Stage by pattern
git add src/**/*.ts

# Interactive staging
git add -p

# Stage part of a file (interactive)
git add -p file.txt
# Then use: y (yes), n (no), s (split), e (edit)
```

### Reviewing Before Staging
```bash
# See unstaged changes
git diff

# See what will be staged
git diff file.txt

# Check status
git status
```

## Amending Commits

### When to Amend
✅ **Safe to amend:**
- Last commit only
- Not yet pushed to remote
- You are the author

❌ **Never amend:**
- Commits already pushed to shared branches
- Commits authored by others
- Commits on main/master

### How to Amend
```bash
# Check if safe to amend
git log -1 --format='%an %ae'  # Check authorship
git status  # Should show "ahead" not "up to date"

# Amend without changing message
git add forgotten-file.ts
git commit --amend --no-edit

# Amend and change message
git commit --amend

# Amend with new message
git commit --amend -m "new message"
```

## Best Practices

### Focus on WHY, Not WHAT
```bash
# Bad: Describes WHAT
git commit -m "add error handling to login function"

# Good: Describes WHY
git commit -m "fix: prevent crashes when user object is undefined

- Resolve application crashes during invalid login attempts
- Add defensive checks at authentication boundaries

Signed-off-by: Vincent Demeester <vincent@sbr.pm>"
```

### Atomic Commits
```bash
# One logical change per commit

# Bad: Multiple unrelated changes
git add src/auth/ src/ui/ docs/
git commit -m "updates"

# Good: Separate commits
git add src/auth/
git commit --signoff -m "feat: add JWT authentication"

git add src/ui/
git commit --signoff -m "feat: add login form"

git add docs/
git commit --signoff -m "docs: document authentication flow"
```

### Commit Often
```bash
# Commit at logical checkpoints
git commit -m "feat: add user model"
git commit -m "feat: add user repository"
git commit -m "feat: add user service"
git commit -m "test: add user service tests"
```

## Mandatory Requirements

### Always Include
1. **--signoff flag** (or configure as default)
2. **Conventional commit type**
3. **WHY-focused bullet points** (not WHAT)

### Never Include
- ❌ Emojis (🚀, ✨, etc.)
- ❌ Generic messages ("updates", "fixes", "WIP")
- ❌ WHAT-focused bullets ("added file X", "changed Y")

## Pre-commit Checks

### Before Committing
```bash
# 1. Review what's staged
git diff --staged

# 2. Check for secrets
grep -r "API_KEY\|SECRET\|PASSWORD" .

# 3. Run tests
go test ./...  # or npm test, etc.

# 4. Run linters
golangci-lint run  # or eslint, etc.

# 5. Verify no debug code
grep -r "console.log\|debugger\|TODO" src/
```

## Interactive Staging

### Patch Mode
```bash
# Stage interactively
git add -p file.txt

# Options:
# y - stage this hunk
# n - don't stage this hunk
# s - split hunk into smaller parts
# e - manually edit hunk
# q - quit
```

### Editing Hunks
```bash
# Manually edit what gets staged
git add -e file.txt

# Or for specific file
git add -p file.txt
# Then press 'e' to edit
```

## Fixing Mistakes

### Unstage Files
```bash
# Unstage specific file
git reset HEAD file.txt

# Unstage all
git reset HEAD

# New syntax (Git 2.23+)
git restore --staged file.txt
```

### Undo Last Commit (Keep Changes)
```bash
git reset --soft HEAD~1
```

### Change Last Commit Message
```bash
git commit --amend
```

## Templates

### Commit Message Template
Create `.gitmessage`:
```
# <type>: <subject> (max 72 chars)
# |<----  Using a Maximum Of 72 Characters  ---->|

# Explain why this change is being made (WHY and IMPACT)
# |<----   Try To Limit Each Line to a Maximum Of 72 Characters   ---->|

# --- COMMIT END ---
# Type can be:
#    feat     (new feature)
#    fix      (bug fix)
#    refactor (refactoring code)
#    style    (formatting, missing semi colons, etc)
#    docs     (changes to documentation)
#    test     (adding or refactoring tests)
#    chore    (maintain)
```

Configure:
```bash
git config --global commit.template ~/.gitmessage
```

## Git Hooks Integration

### Prepare-commit-msg Hook
```bash
# .git/hooks/prepare-commit-msg
#!/bin/bash

# Add branch name to commit message
BRANCH_NAME=$(git symbolic-ref --short HEAD)
if [[ $BRANCH_NAME =~ ^(feat|fix|chore)/ ]]; then
    TYPE=$(echo $BRANCH_NAME | cut -d'/' -f1)
    echo "$TYPE: " > "$1.tmp"
    cat "$1" >> "$1.tmp"
    mv "$1.tmp" "$1"
fi
```

## Configuration

### Useful Git Config
```bash
# Always signoff commits
git config --global format.signoff true

# Set commit template
git config --global commit.template ~/.gitmessage

# Verbose commit (shows diff)
git config --global commit.verbose true

# Set default editor
git config --global core.editor vim
```

## Resources

- [Conventional Commits](https://www.conventionalcommits.org/)
- [How to Write a Git Commit Message](https://chris.beams.io/posts/git-commit/)
- [Git Commit Best Practices](https://github.com/trein/dev-best-practices/wiki/Git-Commit-Best-Practices)
