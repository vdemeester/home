# Cherry-Pick Workflow

Apply specific commits from one branch to another.

## When to Use

- "cherry pick commit"
- "apply specific commit"
- "copy commit to another branch"
- "backport fix"

## Quick Commands

### Basic Cherry-Pick
```bash
# Cherry-pick single commit
git cherry-pick abc1234

# Cherry-pick multiple commits
git cherry-pick abc1234 def5678 ghi9012

# Cherry-pick range of commits (exclusive start)
git cherry-pick abc1234..def5678

# Cherry-pick range (inclusive)
git cherry-pick abc1234^..def5678

# Continue after resolving conflicts
git cherry-pick --continue

# Abort cherry-pick
git cherry-pick --abort

# Skip current commit
git cherry-pick --skip
```

### Cherry-Pick Options
```bash
# Cherry-pick without committing
git cherry-pick -n abc1234
git cherry-pick --no-commit abc1234

# Cherry-pick with different author
git cherry-pick abc1234 --edit

# Cherry-pick and add signoff
git cherry-pick -s abc1234
git cherry-pick --signoff abc1234

# Keep original author
git cherry-pick -x abc1234
# Adds "(cherry picked from commit abc1234)"
```

## When to Cherry-Pick

### Good Use Cases

**Backporting Bug Fixes**
```bash
# Fix was committed to main
# Need same fix in release branch

git checkout release/v1.0
git cherry-pick <fix-commit-hash>
```

**Applying Specific Features**
```bash
# Large feature branch, but only need one commit
# Instead of merging entire branch

git checkout develop
git cherry-pick <specific-feature-commit>
```

**Emergency Hotfixes**
```bash
# Critical fix needed in multiple branches

git checkout main
# Fix and commit
git checkout develop
git cherry-pick <fix-commit>
git checkout release/v2.0
git cherry-pick <fix-commit>
```

### When NOT to Cherry-Pick

❌ **Regular merging**: Use merge or rebase instead
❌ **Multiple dependent commits**: Merge the branch instead
❌ **Entire feature branches**: Use merge
❌ **Maintaining parallel branches**: Use merge to avoid divergence

## Cherry-Pick Process

### Step-by-Step Example
```bash
# 1. Find commit to cherry-pick
git log --oneline feature/auth
# abc1234 fix: prevent null pointer in login

# 2. Switch to target branch
git checkout develop

# 3. Cherry-pick the commit
git cherry-pick abc1234

# 4. If conflicts occur:
# Auto-merging src/auth.ts
# CONFLICT (content): Merge conflict in src/auth.ts

# 5. Resolve conflicts
vim src/auth.ts
# Fix conflicts manually

# 6. Stage resolved files
git add src/auth.ts

# 7. Continue cherry-pick
git cherry-pick --continue

# 8. Verify
git log -1
git show HEAD
```

## Resolving Conflicts

### Conflict Resolution Process
```bash
# Cherry-pick causes conflict
git cherry-pick abc1234
# CONFLICT (content): Merge conflict in file.txt

# See conflicting files
git status

# View conflict
git diff

# Edit and resolve
vim file.txt

# Stage resolution
git add file.txt

# Continue
git cherry-pick --continue

# Or abort if needed
git cherry-pick --abort
```

### Conflict Markers
```javascript
<<<<<<< HEAD (current branch)
const API_URL = "https://api.example.com/v1";
=======
const API_URL = "https://api.example.com/v2";  // from cherry-picked commit
>>>>>>> abc1234 (fix: update API version)
```

Resolve to:
```javascript
const API_URL = "https://api.example.com/v2";
```

## Cherry-Pick Multiple Commits

### Individual Commits
```bash
# Cherry-pick multiple specific commits
git cherry-pick abc1234 def5678 ghi9012

# If conflicts in first commit:
# Resolve and continue
git add conflicted-file.txt
git cherry-pick --continue

# Process continues with next commit
```

### Commit Range
```bash
# Cherry-pick range (abc1234 not included)
git cherry-pick abc1234..def5678

# Cherry-pick range (abc1234 included)
git cherry-pick abc1234^..def5678

# View commits that will be picked
git log --oneline abc1234..def5678
```

## Cherry-Pick Options

### No Commit (-n)
```bash
# Apply changes without committing
git cherry-pick -n abc1234

# Changes staged but not committed
git status
# Changes to be committed:
#   modified: file.txt

# Make additional changes if needed
vim file.txt

# Commit when ready
git commit
```

### Edit Message (-e)
```bash
# Cherry-pick and edit commit message
git cherry-pick -e abc1234

# Opens editor with original message
# Modify as needed and save
```

### Add Note (-x)
```bash
# Add reference to original commit
git cherry-pick -x abc1234

# Commit message becomes:
# fix: original message
#
# (cherry picked from commit abc1234def5678...)
```

### Signoff (-s)
```bash
# Add signoff line
git cherry-pick -s abc1234

# Adds to commit message:
# Signed-off-by: Your Name <your@email.com>
```

### Mainline (-m)
```bash
# Cherry-pick a merge commit
# Must specify which parent to follow

git cherry-pick -m 1 <merge-commit>
# -m 1: Use first parent (usually main branch)
# -m 2: Use second parent (usually feature branch)
```

## Common Workflows

### Backport to Release Branch
```bash
# Fix committed to main
git checkout main
git log --oneline
# abc1234 fix: critical security issue

# Apply to release branch
git checkout release/v2.0
git cherry-pick abc1234

# Add note about backport
git commit --amend -m "fix: critical security issue

Backported from main (abc1234)

Signed-off-by: Your Name <your@email.com>"

# Push to release
git push origin release/v2.0
```

### Selective Feature Migration
```bash
# Feature branch has 10 commits
# Only want commits 3, 5, and 7

git log --oneline feature/new-ui
# aaa commit 10
# bbb commit 9
# ccc commit 8
# ddd commit 7  ← want this
# eee commit 6
# fff commit 5  ← want this
# ggg commit 4
# hhh commit 3  ← want this
# iii commit 2
# jjj commit 1

git checkout develop
git cherry-pick hhh fff ddd
```

### Apply Same Fix to Multiple Branches
```bash
# Fix in develop
git checkout develop
git commit -m "fix: memory leak in parser"
# abc1234

# Apply to release branches
git checkout release/v1.0
git cherry-pick abc1234

git checkout release/v2.0
git cherry-pick abc1234

git checkout main
git cherry-pick abc1234
```

## Cherry-Pick and Merge Conflicts

### Strategy Selection
```bash
# Use recursive strategy with ours/theirs
git cherry-pick -X ours abc1234     # Prefer current branch
git cherry-pick -X theirs abc1234   # Prefer cherry-picked changes
```

### Complex Conflicts
```bash
# Start cherry-pick
git cherry-pick abc1234
# Multiple conflicts

# Resolve each file
git status  # See all conflicts
# both modified: file1.txt
# both modified: file2.txt

# Resolve one by one
vim file1.txt
git add file1.txt

vim file2.txt
git add file2.txt

# Verify all resolved
git status

# Continue
git cherry-pick --continue
```

## Cherry-Pick from Another Repository

### Using Remote
```bash
# Add other repository as remote
git remote add other-repo https://github.com/user/other-repo.git

# Fetch commits
git fetch other-repo

# Cherry-pick commit
git cherry-pick other-repo/main~3
```

### Using Patch
```bash
# In source repository
git format-patch -1 abc1234
# Creates 0001-commit-message.patch

# Copy patch to target repository
# In target repository
git apply 0001-commit-message.patch
git commit
```

## Undoing Cherry-Pick

### Before Committing
```bash
# Cherry-pick in progress
git cherry-pick --abort
```

### After Committing
```bash
# Remove last cherry-picked commit
git reset --hard HEAD~1

# Or revert the cherry-pick
git revert HEAD
```

## Cherry-Pick Best Practices

1. **Document the cherry-pick**: Use `-x` flag to add reference
2. **Test after cherry-pick**: Code may behave differently in target branch
3. **Prefer merge for features**: Cherry-pick for fixes, not feature development
4. **Consider dependencies**: Ensure cherry-picked commit doesn't depend on others
5. **Clean commit history**: Cherry-pick atomic commits, not messy WIP commits
6. **Communicate with team**: Let others know about cherry-picks to shared branches
7. **Avoid duplicate commits**: Don't cherry-pick then merge same commits later

## Advanced Techniques

### Cherry-Pick Range Excluding Commits
```bash
# Pick all except specific commits
git cherry-pick main~10..main

# Skip specific commits during range
git cherry-pick main~10..main
# When specific commit causes conflict you want to skip:
git cherry-pick --skip
```

### Interactive Cherry-Pick
```bash
# Cherry-pick without committing
git cherry-pick -n abc1234

# Review changes
git diff --staged

# Make modifications
vim file.txt

# Commit with modified changes
git commit
```

### Cherry-Pick Merge Commits
```bash
# See merge commit parents
git show --format="%P" <merge-commit>

# Cherry-pick merge commit
# -m 1: Use first parent (main branch)
# -m 2: Use second parent (feature branch)
git cherry-pick -m 1 <merge-commit>
```

## Troubleshooting

### Empty Commit
```bash
# Cherry-pick results in no changes
git cherry-pick abc1234
# The previous cherry-pick is now empty, possibly due to conflict resolution.

# Allow empty commit
git cherry-pick --allow-empty abc1234

# Or skip
git cherry-pick --skip
```

### Wrong Branch
```bash
# Cherry-picked to wrong branch
# Before pushing:
git reset --hard HEAD~1
git checkout correct-branch
git cherry-pick abc1234
```

### Duplicate Commits
```bash
# Find duplicate commits
git log --oneline --all --graph | grep "same message"

# Avoid by checking before cherry-pick
git log --oneline | grep "message to cherry-pick"
```

## Cherry-Pick vs Other Approaches

### Cherry-Pick vs Merge
| Cherry-Pick | Merge |
|-------------|-------|
| Specific commits | Entire branch |
| Creates new commits | Preserves history |
| Can cause duplicates | Tracks branch relationships |
| Good for fixes | Good for features |

### Cherry-Pick vs Rebase
| Cherry-Pick | Rebase |
|-------------|--------|
| Apply to different branch | Replay on same lineage |
| Select specific commits | All commits |
| No history rewrite | Rewrites history |
| Safe for public branches | Risky for public branches |

## Resources

- [Git Cherry-Pick Documentation](https://git-scm.com/docs/git-cherry-pick)
- [Cherry-Pick Guide](https://www.atlassian.com/git/tutorials/cherry-pick)
- [When to Cherry-Pick](https://www.git-tower.com/learn/git/faq/cherry-pick)
