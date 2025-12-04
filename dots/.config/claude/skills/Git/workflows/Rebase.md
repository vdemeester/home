# Rebase Workflow

Rebase commits for a cleaner history and easier code review.

## When to Use

- "rebase git branch"
- "interactive rebase"
- "clean up commits"
- "rebase on main"

## Quick Commands

### Basic Rebase
```bash
# Rebase current branch on main
git rebase main

# Rebase on remote main
git fetch origin
git rebase origin/main

# Continue after resolving conflicts
git rebase --continue

# Skip current commit
git rebase --skip

# Abort rebase
git rebase --abort
```

### Interactive Rebase
```bash
# Rebase last 3 commits
git rebase -i HEAD~3

# Rebase all commits on main
git rebase -i main

# Rebase from specific commit
git rebase -i abc1234
```

## Interactive Rebase Commands

```
pick   - keep commit as-is
reword - change commit message
edit   - stop to amend commit
squash - merge into previous commit (keep both messages)
fixup  - merge into previous commit (discard this message)
drop   - remove commit
```

### Example Interactive Rebase
```bash
# Start interactive rebase
git rebase -i HEAD~4

# Editor shows:
pick a1b2c3d feat: add user model
pick d4e5f6g fix: typo in user model
pick g7h8i9j feat: add user repository
pick j0k1l2m test: add user tests

# Change to:
pick a1b2c3d feat: add user model
fixup d4e5f6g fix: typo in user model
pick g7h8i9j feat: add user repository
pick j0k1l2m test: add user tests

# Result: 4 commits → 3 commits (typo fix merged into original)
```

## Resolving Conflicts

### Conflict Resolution Process
```bash
# 1. Rebase encounters conflict
git rebase origin/main
# CONFLICT (content): Merge conflict in file.txt

# 2. See conflicting files
git status

# 3. Edit files to resolve conflicts
# Remove conflict markers: <<<<<<<, =======, >>>>>>>

# 4. Stage resolved files
git add file.txt

# 5. Continue rebase
git rebase --continue

# 6. Repeat if more conflicts
```

### Conflict Markers
```
<<<<<<< HEAD (your changes)
const name = "Alice";
=======
const name = "Bob";
>>>>>>> abc1234 (incoming changes)
```

Resolve to:
```javascript
const name = "Alice";  // or "Bob", or merge both
```

## Common Rebase Scenarios

### Update Feature Branch
```bash
# Keep feature branch up to date with main
git checkout feature/my-feature
git fetch origin
git rebase origin/main

# If conflicts, resolve and continue
git add resolved-file.txt
git rebase --continue
```

### Squash Commits Before Merge
```bash
# Squash all commits into one
git rebase -i main

# In editor, mark all but first as 'squash' or 'fixup'
pick abc1234 feat: initial implementation
squash def5678 fix: address review comments
squash ghi9012 fix: another typo

# Results in single clean commit
```

### Split a Commit
```bash
# Start interactive rebase
git rebase -i HEAD~3

# Mark commit as 'edit'
edit abc1234 feat: add multiple features

# When rebase stops
git reset HEAD^  # Unstage everything
git add file1.txt
git commit -m "feat: add feature 1"
git add file2.txt
git commit -m "feat: add feature 2"

# Continue rebase
git rebase --continue
```

## Rebase vs Merge

### Use Rebase When
- Working on feature branch
- Want linear history
- Haven't pushed yet (or force push is acceptable)
- Cleaning up commits before PR

### Use Merge When
- On main/master branch
- Commits already pushed to shared branch
- Want to preserve exact history
- Multiple people working on same branch

## Force Push After Rebase

```bash
# Rebase rewrites history, requires force push
git push --force-with-lease

# Safer than --force (checks remote hasn't changed)
# Recommended over:
git push --force  # Dangerous, can overwrite others' work
```

## Rebase Safety

### Before Rebasing
```bash
# 1. Check you're on correct branch
git branch --show-current

# 2. Commit or stash changes
git status
git stash  # if needed

# 3. Fetch latest changes
git fetch origin

# 4. Create backup branch (optional but safe)
git branch backup-$(git branch --show-current)
```

### Recovery If Needed
```bash
# Find lost commits
git reflog

# Recover to previous state
git reset --hard HEAD@{5}  # or specific commit

# Restore from backup branch
git reset --hard backup-feature-branch
```

## Best Practices

1. **Never rebase public branches**: Don't rebase main/master
2. **Rebase before pushing**: Clean up local commits first
3. **Use --force-with-lease**: Safer than --force
4. **Communicate with team**: If rebasing shared branch
5. **Fetch before rebase**: Ensure you have latest changes
6. **Test after rebase**: Run tests to ensure nothing broken
7. **One logical change per commit**: Makes rebasing easier

## Advanced Techniques

### Autosquash
```bash
# Create fixup commit
git commit --fixup=abc1234

# Rebase with autosquash
git rebase -i --autosquash main
# Automatically marks fixup commits
```

### Preserve Merge Commits
```bash
# Rebase keeping merge commits
git rebase --rebase-merges main
```

### Rebase Onto
```bash
# Move commits to different base
git rebase --onto main feature-a feature-b
```

## Resources

- [Git Rebase Documentation](https://git-scm.com/docs/git-rebase)
- [Rewriting History](https://git-scm.com/book/en/v2/Git-Tools-Rewriting-History)
- [Merging vs Rebasing](https://www.atlassian.com/git/tutorials/merging-vs-rebasing)
