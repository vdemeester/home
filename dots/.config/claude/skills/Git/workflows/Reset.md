# Reset Workflow

Undo changes and move branch pointers using Git reset.

## When to Use

- "undo git commit"
- "reset git branch"
- "discard changes"
- "move branch pointer"

## Quick Commands

### Basic Reset
```bash
# Undo last commit, keep changes staged
git reset --soft HEAD~1

# Undo last commit, keep changes unstaged
git reset --mixed HEAD~1
git reset HEAD~1  # mixed is default

# Undo last commit, discard all changes
git reset --hard HEAD~1

# Reset to specific commit
git reset --hard abc1234

# Unstage files
git reset HEAD file.txt
git reset  # Unstage all
```

## Reset Modes

### --soft (Keep Everything)
```bash
# Undo commit, keep changes staged
git reset --soft HEAD~1

# Result:
# - Commit removed from history
# - Changes stay staged
# - Working directory unchanged

# Use case: Redo commit with different message
git reset --soft HEAD~1
git commit -m "better message"
```

### --mixed (Keep Working Changes)
```bash
# Undo commit, unstage changes
git reset --mixed HEAD~1
git reset HEAD~1  # Same (default mode)

# Result:
# - Commit removed from history
# - Changes unstaged
# - Working directory unchanged

# Use case: Re-stage files differently
git reset HEAD~1
git add specific-files.txt
git commit
```

### --hard (Discard Everything)
```bash
# Undo commit, discard all changes
git reset --hard HEAD~1

# Result:
# - Commit removed from history
# - Staging area cleared
# - Working directory reverted

# ⚠️ WARNING: Loses uncommitted changes!

# Use case: Completely abandon recent work
git reset --hard HEAD~1
```

## Common Use Cases

### Undo Last Commit (Keep Changes)
```bash
# Made commit too early
git commit -m "incomplete work"

# Undo commit, keep changes staged
git reset --soft HEAD~1

# Continue working
vim file.txt
git add .
git commit -m "complete work"
```

### Undo Multiple Commits
```bash
# Undo last 3 commits, keep changes
git reset --soft HEAD~3

# Or undo to specific commit
git reset --soft abc1234

# All changes from 3 commits now staged
git status
```

### Unstage Files
```bash
# Staged wrong files
git add .
git status
# Changes to be committed:
#   modified: wanted.txt
#   modified: unwanted.txt

# Unstage specific file
git reset HEAD unwanted.txt

# Or unstage all
git reset HEAD
```

### Discard All Local Changes
```bash
# Working directory is messy
# Want to start fresh from last commit

git reset --hard HEAD

# ⚠️ All uncommitted changes lost!
```

### Move Branch to Different Commit
```bash
# Branch points to wrong commit
git reset --hard abc1234

# Branch now points to abc1234
# All commits after abc1234 are "removed"
# (recoverable via reflog)
```

## Reset Safety

### Before Using --hard
```bash
# Check what will be lost
git diff HEAD
git status

# Create backup branch
git branch backup-before-reset

# Now safe to reset
git reset --hard HEAD~5

# If needed, restore
git reset --hard backup-before-reset
```

### Recovering from Reset
```bash
# Find lost commits
git reflog
# abc1234 HEAD@{0}: reset: moving to HEAD~3
# def5678 HEAD@{1}: commit: important work
# ghi9012 HEAD@{2}: commit: more work

# Restore to before reset
git reset --hard HEAD@{1}
# Or
git reset --hard def5678
```

## Reset vs Revert

### Reset (Rewrite History)
```bash
# Remove commits from history
git reset --hard HEAD~2

# ⚠️ Changes history
# ✓ Clean history
# ✗ Dangerous on shared branches
```

### Revert (Preserve History)
```bash
# Create new commit that undoes changes
git revert HEAD

# ✓ Safe for shared branches
# ✓ Preserves history
# ✗ Creates additional commit
```

### When to Use Each
| Scenario | Use Reset | Use Revert |
|----------|-----------|------------|
| Local commits not pushed | ✓ | |
| Already pushed to shared branch | | ✓ |
| Want clean history | ✓ | |
| Want to preserve history | | ✓ |
| Undoing merge commits | | ✓ |

## Reset Specific Files

### Unstage Files
```bash
# Unstage specific file
git reset HEAD file.txt

# Unstage all files
git reset HEAD

# New syntax (Git 2.23+)
git restore --staged file.txt
```

### Reset File to Specific Commit
```bash
# Reset file to version from 3 commits ago
git reset HEAD~3 -- file.txt

# File changes staged
# Commit or discard as needed
git commit -m "restore old version of file"
```

## Reset with Paths

### Reset Specific Files to HEAD
```bash
# Discard changes to specific file
git reset --hard HEAD -- file.txt

# Better alternative (clearer intent):
git checkout HEAD -- file.txt
# Or (Git 2.23+):
git restore file.txt
```

## Advanced Reset Techniques

### Reset and Squash
```bash
# Squash last 5 commits into one
git reset --soft HEAD~5
git commit -m "squashed commit message"
```

### Partial Reset
```bash
# Reset some files, keep others
git reset HEAD~1 -- src/
# Only src/ files reset, rest unchanged
```

### Reset to Remote
```bash
# Discard all local commits, match remote
git fetch origin
git reset --hard origin/main

# ⚠️ Loses all local commits!
```

## Reset Scenarios

### Wrong Branch
```bash
# Made commits on main instead of feature branch
git branch feature/my-work  # Save work
git reset --hard origin/main  # Reset main
git checkout feature/my-work  # Continue on feature
```

### Undo Merge
```bash
# Just merged but it was wrong
git reset --hard HEAD~1

# Or if merge not committed yet
git merge --abort
```

### Clean Slate
```bash
# Want to completely match remote
git fetch origin
git reset --hard origin/main
git clean -fdx  # Also remove untracked files
```

## Reset and Reflog

### Viewing Reflog
```bash
# See all HEAD movements
git reflog

# Output:
# abc1234 HEAD@{0}: reset: moving to HEAD~3
# def5678 HEAD@{1}: commit: important work
# ghi9012 HEAD@{2}: commit: feature added
```

### Recovering Lost Commits
```bash
# After bad reset
git reset --hard HEAD~10
# Oh no! Lost important work

# Find it in reflog
git reflog
# def5678 HEAD@{1}: commit: important work

# Restore
git reset --hard def5678
# Or
git reset --hard HEAD@{1}
```

### Reflog Expiration
```bash
# Reflog keeps history for 90 days (default)
# After that, commits are garbage collected

# Immediately expire reflog (dangerous!)
git reflog expire --expire=now --all
git gc --prune=now
```

## Reset Best Practices

1. **Never reset public branches**: Don't reset commits pushed to shared branches
2. **Use --soft for commit cleanup**: Safest way to redo commits
3. **Create backup before --hard**: Use backup branch or check reflog
4. **Prefer revert for public branches**: Safer than reset for shared history
5. **Check diff before --hard**: Know what you're losing
6. **Communicate resets on shared branches**: Team needs to know
7. **Use --force-with-lease after reset**: Safer than --force when pushing

## Dangerous Reset Patterns

### ❌ Reset Public Branch
```bash
# BAD: Others have this commit
git reset --hard HEAD~5
git push --force origin main
# Other developers' work now conflicts
```

### ❌ Reset Without Checking
```bash
# BAD: Don't know what's being lost
git reset --hard HEAD~10
# Oops, lost important work
```

### ✓ Safe Alternative
```bash
# GOOD: Create backup first
git branch backup-main
git reset --hard HEAD~10

# Can restore if needed
git reset --hard backup-main
```

## Reset and Push

### After Local Reset
```bash
# Reset local commits
git reset --hard HEAD~3

# Try to push
git push
# error: Updates were rejected

# Must force push (dangerous!)
git push --force-with-lease
```

### Force Push Safety
```bash
# Safer than --force (checks remote hasn't changed)
git push --force-with-lease

# Protects against overwriting others' work
```

## Reset Alternatives

### For Committed Changes
```bash
# Instead of reset --hard
# Use revert (safe for shared branches)
git revert HEAD
```

### For Uncommitted Changes
```bash
# Instead of reset --hard
# Use stash (recoverable)
git stash
git stash drop  # If sure you don't need it
```

### For Specific Files
```bash
# Instead of reset --hard file.txt
# Use restore (clearer intent, Git 2.23+)
git restore file.txt
git restore --staged file.txt
```

## Configuration

```bash
# Require explicit mode for reset
git config --global advice.resetNoMode true

# Always show what reset will do
git config --global advice.resetQuiet false
```

## Emergency Recovery

### Lost Commits
```bash
# Find all unreachable commits
git fsck --lost-found

# Or search reflog
git reflog --all | grep "search term"

# Recover specific commit
git cherry-pick <lost-commit-hash>
```

### Completely Destroyed Branch
```bash
# Find branch in reflog
git reflog show feature/my-work

# Restore branch
git checkout -b feature/my-work-recovered <commit-hash>
```

## Resources

- [Git Reset Documentation](https://git-scm.com/docs/git-reset)
- [Reset, Checkout, and Revert](https://www.atlassian.com/git/tutorials/resetting-checking-out-and-reverting)
- [Undoing Changes in Git](https://git-scm.com/book/en/v2/Git-Basics-Undoing-Things)
