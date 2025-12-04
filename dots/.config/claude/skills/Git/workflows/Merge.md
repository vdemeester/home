# Merge Workflow

Merge branches with proper strategies and conflict resolution.

## When to Use

- "merge git branch"
- "merge conflicts"
- "resolve conflicts"
- "merge strategies"

## Quick Commands

### Basic Merge
```bash
# Merge feature branch into current branch
git merge feature/user-auth

# Merge with commit message
git merge feature/user-auth -m "Merge user authentication feature"

# No fast-forward (always create merge commit)
git merge --no-ff feature/user-auth

# Abort merge
git merge --abort
```

### Merge Strategies

```bash
# Fast-forward only (fail if not possible)
git merge --ff-only feature/user-auth

# Squash merge (combine all commits)
git merge --squash feature/user-auth
git commit -m "feat: add user authentication"

# No commit (stage changes only)
git merge --no-commit feature/user-auth
```

## Merge vs Fast-Forward

### Fast-Forward Merge
```bash
# When feature is ahead of main, no divergence
git checkout main
git merge feature/simple-fix

# Result: main pointer moves forward, no merge commit
# main:    A---B
# feature:     C---D
# After:   A---B---C---D (main)
```

### Three-Way Merge
```bash
# When both branches have diverged
git checkout main
git merge feature/complex-feature

# Result: creates merge commit
# main:    A---B-------E (merge commit)
#               \     /
# feature:       C---D
```

## Resolving Conflicts

### Conflict Resolution Process
```bash
# 1. Attempt merge
git merge feature/user-auth
# Auto-merging file.txt
# CONFLICT (content): Merge conflict in file.txt

# 2. See conflicting files
git status
# Unmerged paths:
#   both modified:   file.txt

# 3. View conflicts
git diff

# 4. Edit file to resolve
# Remove conflict markers and choose resolution

# 5. Stage resolved files
git add file.txt

# 6. Complete merge
git commit

# Or abort
git merge --abort
```

### Conflict Markers
```
<<<<<<< HEAD (current branch)
const API_URL = "https://api.example.com/v1";
=======
const API_URL = "https://api.example.com/v2";
>>>>>>> feature/api-update
```

### Resolution Options
```javascript
// Option 1: Keep current
const API_URL = "https://api.example.com/v1";

// Option 2: Take incoming
const API_URL = "https://api.example.com/v2";

// Option 3: Combine both
const API_URL = process.env.API_VERSION === "2"
    ? "https://api.example.com/v2"
    : "https://api.example.com/v1";
```

## Merge Tools

### Using Merge Tool
```bash
# Configure merge tool
git config --global merge.tool vimdiff

# Run merge tool for conflicts
git mergetool

# Common merge tools:
# - vimdiff
# - meld
# - kdiff3
# - p4merge
# - code (VS Code)
```

### VS Code as Merge Tool
```bash
git config --global merge.tool vscode
git config --global mergetool.vscode.cmd 'code --wait $MERGED'
```

## Merge Strategies

### Recursive (Default)
```bash
# Default for two branches
git merge -s recursive feature/branch
```

### Ours/Theirs
```bash
# Keep our version on conflict
git merge -X ours feature/branch

# Keep their version on conflict
git merge -X theirs feature/branch
```

### Octopus
```bash
# Merge multiple branches at once
git merge feature/auth feature/api feature/ui
```

## Squash Merge

### When to Use
- Feature branch with many small commits
- Want clean main branch history
- Don't need to preserve individual commits

### How to Squash Merge
```bash
# 1. Squash merge
git checkout main
git merge --squash feature/many-commits

# 2. All changes staged, no commit created
git status

# 3. Create single commit
git commit -m "feat: complete user authentication

- Add login/logout functionality
- Implement JWT token management
- Add password hashing
- Create user session handling"
```

## Cherry-Pick During Merge

```bash
# If you want specific commits instead of full merge
git cherry-pick abc1234 def5678
```

## Undoing Merges

### Before Committing
```bash
# Abort in-progress merge
git merge --abort

# Reset staged changes
git reset --hard HEAD
```

### After Committing
```bash
# Undo merge commit (keep changes)
git reset --soft HEAD~1

# Undo merge commit (discard changes)
git reset --hard HEAD~1

# Revert merge (safe for public branches)
git revert -m 1 HEAD
# -m 1 means keep main branch side
```

## Best Practices

1. **Always pull before merge**: Ensure you have latest changes
2. **Merge into correct branch**: Double-check current branch
3. **Test before merging**: Run tests on feature branch first
4. **Use --no-ff for features**: Preserve branch history
5. **Squash for cleanup**: Use squash merge for messy branches
6. **Communicate conflicts**: Discuss non-trivial resolutions
7. **Never merge broken code**: Ensure tests pass first

## Common Workflows

### Feature Branch Merge
```bash
# 1. Update main
git checkout main
git pull

# 2. Merge feature
git merge --no-ff feature/user-auth

# 3. Push
git push
```

### Hotfix Merge
```bash
# 1. Create hotfix from main
git checkout -b hotfix/security-patch main

# 2. Fix and commit
git commit -m "fix: security vulnerability"

# 3. Merge to main
git checkout main
git merge --no-ff hotfix/security-patch

# 4. Tag release
git tag -a v1.0.1 -m "Security patch"

# 5. Merge to develop if exists
git checkout develop
git merge --no-ff hotfix/security-patch
```

## Configuration

```bash
# Always create merge commit (no fast-forward)
git config --global merge.ff false

# For pull specifically
git config --global pull.ff only

# Show conflict style
git config --global merge.conflictstyle diff3
```

## Resources

- [Git Merge Documentation](https://git-scm.com/docs/git-merge)
- [Resolving Conflicts](https://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging#_basic_merge_conflicts)
- [Merge Strategies](https://git-scm.com/docs/merge-strategies)
