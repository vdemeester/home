# Worktree Workflow

Work on multiple branches simultaneously using Git worktrees.

## When to Use

- "work on multiple branches"
- "git worktree"
- "parallel development"
- "test multiple versions"

## Quick Commands

### Creating Worktrees
```bash
# Create worktree for new branch
git worktree add ../my-project-feature feature/new-feature

# Create worktree from existing branch
git worktree add ../my-project-bugfix bugfix/issue-123

# Create worktree with new branch from current
git worktree add -b feature/experiment ../my-project-exp

# Create temporary worktree
git worktree add --detach ../my-project-temp HEAD~5
```

### Managing Worktrees
```bash
# List all worktrees
git worktree list

# Remove worktree
git worktree remove ../my-project-feature

# Remove and prune
git worktree prune

# Move worktree
git worktree move ../old-location ../new-location
```

## What Are Worktrees?

### Traditional Workflow Problem
```bash
# Working on feature
vim src/auth.ts

# Urgent bug needs fixing
git stash          # Save work
git checkout main  # Switch branch
git checkout -b hotfix/urgent
# Fix bug...
git checkout feature/auth
git stash pop      # Restore work

# Lots of context switching!
```

### Worktree Solution
```bash
# Main checkout
~/projects/myapp (feature/auth)

# Create worktree for hotfix
git worktree add ../myapp-hotfix hotfix/urgent

# Now you have two directories:
# ~/projects/myapp (feature/auth)
# ~/projects/myapp-hotfix (hotfix/urgent)

# Work in both simultaneously!
cd ../myapp-hotfix
# Fix bug...
git commit
git push

# Return to feature work
cd ~/projects/myapp
# Continue where you left off, no stash needed
```

## Basic Worktree Operations

### Create Worktree
```bash
# Create worktree for existing branch
git worktree add ../myapp-develop develop

# Create worktree with new branch
git worktree add -b feature/new-ui ../myapp-ui

# Create from specific commit
git worktree add ../myapp-v1 v1.0.0

# Create detached HEAD (no branch)
git worktree add --detach ../myapp-temp abc1234
```

### List Worktrees
```bash
# Show all worktrees
git worktree list

# Output:
# /home/user/myapp         abc1234 [main]
# /home/user/myapp-dev     def5678 [develop]
# /home/user/myapp-hotfix  ghi9012 [hotfix/urgent]

# Verbose output
git worktree list --porcelain
```

### Remove Worktree
```bash
# Remove worktree (directory must be clean)
git worktree remove ../myapp-hotfix

# Force remove (even with uncommitted changes)
git worktree remove --force ../myapp-hotfix

# Remove worktree directory manually
rm -rf ../myapp-hotfix
# Then clean up Git's internal state
git worktree prune
```

## Common Use Cases

### Parallel Feature Development
```bash
# Main feature work
~/projects/myapp (feature/user-auth)

# Create worktree for related feature
git worktree add -b feature/user-profile ../myapp-profile

# Work on both features simultaneously
cd ../myapp-profile
# Implement user profile
git commit

cd ~/projects/myapp
# Continue auth work
```

### Code Review
```bash
# Continue working on your feature
~/projects/myapp (feature/my-work)

# Create worktree to review PR
git fetch origin pull/123/head:pr-123
git worktree add ../myapp-review pr-123

# Review in separate directory
cd ../myapp-review
# Test, read code...

# No need to stash or commit WIP
# Your main work directory unchanged
```

### Testing Branches
```bash
# Create worktree for testing
git worktree add ../myapp-test feature/experimental

cd ../myapp-test
npm install
npm test
npm start  # Test in browser

# If it works, merge from main worktree
cd ~/projects/myapp
git merge feature/experimental

# If it doesn't work, just remove
git worktree remove ../myapp-test
```

### Release Preparation
```bash
# Main development continues
~/projects/myapp (develop)

# Create worktree for release
git worktree add -b release/v2.0 ../myapp-release

cd ../myapp-release
# Prepare release: version bumps, changelog
git commit -m "chore: prepare v2.0 release"

# Development continues in main worktree
cd ~/projects/myapp
# Continue feature work uninterrupted
```

## Worktree Directory Structure

### Recommended Layout
```bash
~/projects/myapp/
  .git/
  main/                      # main branch
  worktrees/
    develop/                 # develop branch
    feature-x/               # feature branch
    hotfix/                  # hotfix branch
```

### Creating Organized Structure
```bash
# From main repository
cd ~/projects/myapp

# Create worktree subdirectory
mkdir -p worktrees

# Add worktrees to subdirectory
git worktree add worktrees/develop develop
git worktree add -b feature/ui worktrees/feature-ui
```

## Worktree Best Practices

### Naming Conventions
```bash
# Use descriptive names matching branch
git worktree add ../myapp-feature-auth feature/auth
git worktree add ../myapp-bugfix-login bugfix/login-error
git worktree add ../myapp-release-v2 release/v2.0

# Or use prefix pattern
git worktree add ../feature-auth feature/auth
git worktree add ../bugfix-login bugfix/login-error
```

### Cleanup Strategy
```bash
# Regularly review worktrees
git worktree list

# Remove finished worktrees
git worktree remove ../myapp-feature-done

# Prune stale references
git worktree prune

# Script to clean old worktrees
#!/bin/bash
git worktree list --porcelain | grep -A2 "^worktree" | while read line; do
    if [[ $line =~ ^worktree ]]; then
        path=$(echo $line | awk '{print $2}')
        if [ ! -d "$path" ]; then
            echo "Removing stale worktree: $path"
            git worktree prune
        fi
    fi
done
```

## Working with Worktrees

### Independent Operations
```bash
# Each worktree can:
# - Have different uncommitted changes
# - Be on different branches
# - Have different stashes
# - Run different processes

# Example: Run different versions
cd ~/projects/myapp
npm start  # Start main branch on port 3000

cd ~/projects/myapp-v2
npm start -- --port 3001  # Start v2 on port 3001

# Compare both versions side-by-side
```

### Shared Repository State
```bash
# All worktrees share:
# - Commit history
# - Branches
# - Tags
# - Remote tracking branches
# - Configuration

# Example: Fetch affects all worktrees
cd ~/projects/myapp
git fetch origin

cd ~/projects/myapp-dev
# Fetch results visible here too
git log origin/main  # Shows newly fetched commits
```

### Restrictions
```bash
# Cannot checkout same branch in multiple worktrees
git worktree add ../myapp-2 feature/auth
# error: 'feature/auth' is already checked out at '/home/user/myapp'

# Workaround: Create new branch from same commit
git worktree add -b feature/auth-copy ../myapp-2 feature/auth
```

## Advanced Worktree Techniques

### Temporary Worktrees for Testing
```bash
# Quick test of a commit
git worktree add --detach /tmp/test-commit abc1234
cd /tmp/test-commit
# Run tests...
cd -
git worktree remove /tmp/test-commit
```

### Worktree for Each PR
```bash
#!/bin/bash
# checkout-pr.sh <PR-number>

PR=$1
WORKTREE_PATH="../myapp-pr-$PR"

# Fetch PR
git fetch origin pull/$PR/head:pr-$PR

# Create worktree
git worktree add "$WORKTREE_PATH" pr-$PR

echo "PR #$PR checked out to: $WORKTREE_PATH"
cd "$WORKTREE_PATH"
```

### Lock Worktree
```bash
# Prevent worktree from being removed
git worktree lock ../myapp-important
# With reason
git worktree lock ../myapp-important --reason "Long-running process"

# Unlock
git worktree unlock ../myapp-important

# List shows locked status
git worktree list
```

## Worktree with Build Artifacts

### Separate Build Directories
```bash
# Main worktree
~/projects/myapp/ (develop)
  src/
  build/  # Build outputs

# Feature worktree with own build
~/projects/myapp-feature/ (feature/new-ui)
  src/
  build/  # Different build outputs

# Each has independent node_modules, build artifacts
```

### Shared Dependencies
```bash
# Share node_modules to save space
cd ~/projects/myapp-feature
ln -s ../myapp/node_modules .

# Or use pnpm which shares by default
cd ~/projects/myapp
pnpm install

cd ~/projects/myapp-feature
pnpm install  # Reuses packages from store
```

## Migration from Stash to Worktree

### Before (Stash Workflow)
```bash
# Working on feature
vim src/feature.ts

# Need to fix bug
git stash
git checkout main
git checkout -b hotfix/bug

# Fix bug
git commit
git checkout feature/work
git stash pop
```

### After (Worktree Workflow)
```bash
# Working on feature
~/projects/myapp (feature/work)
vim src/feature.ts

# Need to fix bug - create worktree
git worktree add ../myapp-hotfix -b hotfix/bug main

# Fix in worktree
cd ../myapp-hotfix
# Fix bug
git commit

# Return to feature (no stash/pop needed!)
cd ~/projects/myapp
# Continue working, all changes intact
```

## Troubleshooting

### Worktree Directory Moved
```bash
# If you moved worktree directory manually
git worktree list
# Shows old path

# Update worktree location
git worktree repair ../new-location
```

### Cannot Remove Worktree
```bash
# Error: Cannot remove worktree with uncommitted changes
git worktree remove ../myapp-feature
# error: --force to override

# Force remove
git worktree remove --force ../myapp-feature

# Or commit/stash changes first
cd ../myapp-feature
git stash
cd -
git worktree remove ../myapp-feature
```

### Stale Worktree References
```bash
# Worktree directory deleted manually
rm -rf ../myapp-old

# Clean up Git state
git worktree prune

# Verify
git worktree list
```

## Worktree vs Other Approaches

### Worktree vs Stash
| Worktree | Stash |
|----------|-------|
| Separate directory | Same directory |
| Multiple branches simultaneously | One branch at a time |
| No context switching | Requires stash/pop |
| More disk space | Less disk space |
| Better for long-term parallel work | Better for quick switches |

### Worktree vs Clone
| Worktree | Clone |
|----------|-------|
| Shares `.git` directory | Independent `.git` |
| Less disk space | More disk space |
| Shared configuration | Independent configuration |
| Cannot checkout same branch | Can checkout same branch |
| Faster to create | Slower to create |

## Configuration

```bash
# Default path for new worktrees
git config worktree.guessRemote true

# Automatically prune worktrees on fetch
git config fetch.prune true
git config fetch.pruneTags true
```

## Resources

- [Git Worktree Documentation](https://git-scm.com/docs/git-worktree)
- [Git Worktree Tutorial](https://morgan.cugerone.com/blog/how-to-use-git-worktree-and-in-a-clean-way/)
- [Parallel Git Workflows](https://www.gitkraken.com/learn/git/git-worktree)
