# Branch Workflow

Manage Git branches effectively with naming conventions and cleanup strategies.

## When to Use

- "create git branch"
- "switch branches"
- "delete branch"
- "list branches"

## Quick Commands

### Creating Branches
```bash
# Create and switch to new branch
git checkout -b feature/user-auth

# Create branch without switching
git branch feature/user-auth

# Create from specific commit
git checkout -b bugfix/issue-123 abc1234

# New syntax (Git 2.23+)
git switch -c feature/user-auth
```

### Switching Branches
```bash
# Switch to existing branch
git checkout main
git switch main  # Git 2.23+

# Switch to previous branch
git checkout -
git switch -

# Switch and create if doesn't exist
git checkout -b feature/new-feature
```

### Listing Branches
```bash
# List local branches
git branch

# List all branches (local + remote)
git branch -a

# List with last commit
git branch -v

# List merged branches
git branch --merged

# List unmerged branches
git branch --no-merged

# List branches with pattern
git branch --list 'feature/*'
```

## Branch Naming Conventions

### Standard Format
```
<type>/<description>
```

### Types
- `feature/` - New features
- `fix/` or `bugfix/` - Bug fixes
- `hotfix/` - Urgent production fixes
- `chore/` - Maintenance tasks
- `docs/` - Documentation
- `refactor/` - Code refactoring
- `test/` - Test additions/fixes

### Examples
```bash
git checkout -b feature/user-authentication
git checkout -b fix/null-pointer-login
git checkout -b hotfix/security-patch
git checkout -b chore/update-dependencies
git checkout -b docs/api-documentation
git checkout -b refactor/simplify-handlers
```

## Deleting Branches

### Local Branches
```bash
# Delete merged branch (safe)
git branch -d feature/completed

# Force delete (even if unmerged)
git branch -D feature/abandoned

# Delete multiple branches
git branch -d feature/old-1 feature/old-2
```

### Remote Branches
```bash
# Delete remote branch
git push origin --delete feature/old-branch

# Alternative syntax
git push origin :feature/old-branch
```

### Bulk Cleanup
```bash
# Delete all merged local branches
git branch --merged | grep -v "\*\|main\|master" | xargs -n 1 git branch -d

# Delete all local branches except main/master
git branch | grep -v "main\|master" | xargs git branch -D

# Prune deleted remote branches
git fetch --prune
git remote prune origin
```

## Remote Branch Tracking

### Setting Upstream
```bash
# Push and set upstream
git push -u origin feature/new-feature

# Set upstream for existing branch
git branch --set-upstream-to=origin/feature/new-feature

# Short form
git branch -u origin/feature/new-feature
```

### Tracking Info
```bash
# Show tracking branches
git branch -vv

# Check remote tracking
git remote show origin
```

## Renaming Branches

### Local Branch
```bash
# Rename current branch
git branch -m new-name

# Rename other branch
git branch -m old-name new-name
```

### Remote Branch
```bash
# Rename local, delete old remote, push new
git branch -m old-name new-name
git push origin --delete old-name
git push -u origin new-name
```

## Branch Management Strategies

### Keep Branches Updated
```bash
# Update main before creating branch
git checkout main
git pull
git checkout -b feature/new-work

# Regularly sync with main
git fetch origin
git rebase origin/main
```

### Clean Gone Branches
```bash
# List gone branches
git branch -vv | grep ': gone]'

# Delete all gone branches
git fetch --prune
git branch -vv | grep ': gone]' | awk '{print $1}' | xargs git branch -D
```

## Best Practices

1. **Use descriptive names**: `feature/add-user-auth` not `feature/fix`
2. **Keep branches short-lived**: Merge or delete within days/weeks
3. **One concern per branch**: Don't mix features and fixes
4. **Update regularly**: Rebase on main frequently
5. **Delete after merge**: Clean up merged branches
6. **Use consistent naming**: Follow team conventions
7. **Protect main/master**: Use branch protection rules

## Resources

- [Git Branch Documentation](https://git-scm.com/docs/git-branch)
- [Branch Naming Conventions](https://dev.to/varbsan/a-simplified-convention-for-naming-branches-and-commits-in-git-il4)
