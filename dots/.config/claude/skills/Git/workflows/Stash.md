# Stash Workflow

Temporarily save changes without committing.

## When to Use

- "stash git changes"
- "save work in progress"
- "temporarily save changes"
- "switch branches with uncommitted changes"

## Quick Commands

### Basic Stash
```bash
# Stash current changes
git stash

# Stash with message
git stash push -m "WIP: working on auth feature"

# Stash including untracked files
git stash -u

# Stash including untracked and ignored files
git stash -a

# Stash specific files
git stash push -m "temp changes" file1.txt file2.txt
```

### Viewing Stashes
```bash
# List all stashes
git stash list

# Show stash contents
git stash show

# Show specific stash
git stash show stash@{1}

# Show diff
git stash show -p
git stash show -p stash@{1}
```

### Applying Stashes
```bash
# Apply most recent stash (keep stash)
git stash apply

# Apply specific stash
git stash apply stash@{2}

# Apply and remove stash
git stash pop

# Apply specific and remove
git stash pop stash@{1}
```

### Managing Stashes
```bash
# Delete specific stash
git stash drop stash@{0}

# Delete all stashes
git stash clear

# Create branch from stash
git stash branch feature/new-work stash@{0}
```

## Common Use Cases

### Quick Context Switch
```bash
# You're working on feature A
# Urgent bug needs fixing

# 1. Stash current work
git stash push -m "WIP: feature A implementation"

# 2. Switch to main and create hotfix
git checkout main
git checkout -b hotfix/urgent-bug

# 3. Fix, commit, and merge
git commit -m "fix: urgent bug"
git checkout main
git merge hotfix/urgent-bug

# 4. Return to feature A
git checkout feature/a
git stash pop
```

### Pull with Uncommitted Changes
```bash
# You have uncommitted changes
# Need to pull latest updates

# 1. Stash changes
git stash

# 2. Pull
git pull

# 3. Reapply changes
git stash pop

# Handle conflicts if any
```

### Try Experimental Changes
```bash
# Stash current stable work
git stash push -m "stable implementation"

# Try experimental approach
# ... make changes ...

# If experiment fails, restore stable
git reset --hard HEAD
git stash pop

# If experiment works, keep it and drop stash
git stash drop
```

## Stash with Partial Changes

### Interactive Stash
```bash
# Stash interactively (like git add -p)
git stash -p

# For each hunk:
# y - stash this hunk
# n - don't stash this hunk
# s - split into smaller hunks
# q - quit
```

### Stash Specific Files
```bash
# Stash only certain files
git stash push -m "temp API changes" src/api.ts src/types.ts

# Stash everything except certain files
git stash push -- src/
```

## Working with Multiple Stashes

### Organized Stash Management
```bash
# Create named stashes
git stash push -m "feature: user auth half done"
git stash push -m "experiment: alternative approach"
git stash push -m "bugfix: in progress"

# List with messages
git stash list
# stash@{0}: On main: bugfix: in progress
# stash@{1}: On main: experiment: alternative approach
# stash@{2}: On main: feature: user auth half done

# Apply specific stash
git stash apply stash@{2}
```

### Creating Branches from Stashes
```bash
# Create branch and apply stash
git stash branch feature/auth-completion stash@{0}

# This:
# 1. Creates new branch
# 2. Checks it out
# 3. Applies stash
# 4. Drops stash if successful
```

## Stash Conflicts

### Resolving Stash Conflicts
```bash
# When applying stash causes conflicts
git stash pop
# CONFLICT (content): Merge conflict in file.txt

# 1. Resolve conflicts manually
# 2. Stage resolved files
git add file.txt

# 3. No need to commit, changes are in working directory
# Stash is already removed by pop

# If you used apply instead:
git stash drop stash@{0}
```

## Advanced Stash Techniques

### Stash Keeping Index
```bash
# Stash unstaged changes, keep staged
git stash --keep-index

# Useful when you want to:
# 1. Keep staged changes
# 2. Temporarily remove unstaged
# 3. Run tests on staged only
```

### Stash Untracked Files Only
```bash
# Stash only untracked files
git stash -u --keep-index
```

### Recover Dropped Stash
```bash
# If you accidentally dropped a stash
# Find it in reflog
git fsck --unreachable | grep commit | cut -d' ' -f3 | xargs git log --merges --no-walk --grep=WIP

# Or
git log --graph --oneline --decorate $(git fsck --no-reflog | awk '/dangling commit/ {print $3}')

# Recover with:
git stash apply <commit-hash>
```

## Stash Workflow Patterns

### Daily Work Pattern
```bash
# End of day
git stash push -m "EOD: $(date +%Y-%m-%d) work in progress"

# Start of day
git stash list  # Review what was stashed
git stash pop   # Continue work
```

### Code Review Pattern
```bash
# You're working on feature
git stash push -m "WIP: before code review"

# Check out PR for review
git fetch origin pull/123/head:pr-123
git checkout pr-123
# Review...

# Return to your work
git checkout feature/my-work
git stash pop
```

## Best Practices

1. **Always use messages**: `git stash push -m "description"`
2. **Clean up old stashes**: Review and drop unneeded stashes
3. **Prefer commits over stash**: For long-term work
4. **Use branches for experiments**: Better than multiple stashes
5. **Pop when you can**: Don't accumulate many stashes
6. **Include untracked with -u**: If you have new files
7. **Review before popping**: Check stash contents first

## Common Pitfalls

### Forgetting About Stashes
```bash
# Regularly check for stashes
git stash list

# If you see old ones, review and clean up
git stash show stash@{0}
git stash drop stash@{0}
```

### Stashing on Wrong Branch
```bash
# If you stashed on wrong branch
# 1. Go to correct branch
git checkout correct-branch

# 2. Apply stash from other branch
git stash apply stash@{0}

# 3. Go back and drop from original
git checkout original-branch
git stash drop stash@{0}
```

## Configuration

```bash
# Show untracked files in stash show
git config --global stash.showIncludeUntracked true

# Show stash in git log
git config --global log.showSignature false
```

## Resources

- [Git Stash Documentation](https://git-scm.com/docs/git-stash)
- [Stashing and Cleaning](https://git-scm.com/book/en/v2/Git-Tools-Stashing-and-Cleaning)
