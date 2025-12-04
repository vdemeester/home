# Maintainer Workflow

Package maintenance in NixOS/nixpkgs: triaging issues, reviewing PRs, and keeping packages updated.

## When to Use

- "nixpkgs maintainer"
- "package maintenance"
- "triage issues"
- "maintainer responsibilities"

## Quick Reference

### Becoming a Maintainer

```bash
# 1. Add yourself to maintainer-list.nix
vim maintainers/maintainer-list.nix

# 2. Add entry (alphabetically sorted)
yourhandle = {
  name = "Your Name";
  github = "YourGitHubUsername";
  githubId = 123456;
};

# 3. Test the format
nix-build lib/tests/maintainers.nix

# 4. Add yourself to package
vim pkgs/by-name/pa/package-name/package.nix
# In meta.maintainers: [ maintainers.yourhandle ]

# 5. Create PR
git commit -s -m "maintainers: add yourhandle"
```

### Ofborg Commands

```bash
# Build specific packages
@ofborg build package-name

# Run NixOS tests
@ofborg test test-name

# Re-evaluate (if evaluation failed)
@ofborg eval

# Multiple commands
@ofborg build hello @ofborg test nginx
```

## Maintainer Responsibilities

### Primary Role

**Main responsibility**: Keep packages in a functioning state and keep up with updates.

Maintainers have decision-making power over their packages but are not the sole contributors. Anyone can propose changes, including bots and other contributors.

### Key Duties

1. **Monitor Updates** - Watch for new upstream releases
2. **Review PRs** - Review and merge changes to maintained packages
3. **Triage Issues** - Address bugs and feature requests
4. **Keep Packages Working** - Fix breakages and test failures
5. **Respond to Mentions** - Reply to @-mentions within reasonable time
6. **Security Updates** - Prioritize CVE fixes and security patches

## Becoming a Maintainer

### Step 1: Add Yourself to maintainer-list.nix

```nix
# maintainers/maintainer-list.nix
{
  # ...
  yourhandle = {
    # Required
    name = "Your Full Name";
    github = "YourGitHubUsername";
    githubId = 123456;  # Get from https://api.github.com/users/YourGitHubUsername

    # Optional
    email = "you@example.com";
    matrix = "@you:matrix.org";
    keys = [{
      fingerprint = "AAAA BBBB CCCC DDDD EEEE  FFFF 0000 1111 2222 3333";
    }];
  };
}
```

**Finding your GitHub ID**:
```bash
# Using API
curl https://api.github.com/users/YourGitHubUsername | jq .id

# Or use gh CLI
gh api /users/YourGitHubUsername --jq .id
```

**Handle naming**:
- Use lowercase
- If GitHub username starts with number, prefix with underscore
- Keep it simple and related to your GitHub username

### Step 2: Test Format

```bash
# Ensure maintainer-list.nix is valid
nix-build lib/tests/maintainers.nix

# Should complete without errors
```

### Step 3: Add to Package

```nix
# In package definition
meta = with lib; {
  # ...
  maintainers = with maintainers; [ yourhandle ];
};
```

### Step 4: Submit PR

```bash
git add maintainers/maintainer-list.nix
git commit -s -m "maintainers: add yourhandle"
git push
gh pr create --title "maintainers: add yourhandle" --body "Adding myself as a maintainer for package-name"
```

### What Happens Next

After PR is merged:
- Invited to @NixOS/nixpkgs-maintainers GitHub team
- Eligible for package review requests
- Part of CI review process
- Can use nixpkgs-merge-bot for your packages

## Using Ofborg

Ofborg is the CI bot that automatically builds and tests packages.

### Automatic Checks

Ofborg automatically runs these checks on every PR:

1. **ofborg-eval**: Validates Nix expression syntax
2. **ofborg-eval-check-maintainers**: Verifies maintainers exist
3. **ofborg-eval-check-meta**: Ensures meta attributes present
4. **ofborg-eval-darwin**: Checks macOS builds
5. **ofborg-eval-nixos**: Checks NixOS builds

### Manual Commands

Trigger ofborg in PR comments:

#### Build Packages

```bash
# Build single package
@ofborg build package-name

# Build multiple packages
@ofborg build package1 package2 package3

# Build with attribute path
@ofborg build pythonPackages.requests

# Ofborg will run:
# nix-build ./default.nix -A package-name
```

#### Run Tests

```bash
# Run specific test
@ofborg test nginx

# Run multiple tests
@ofborg test nginx apache

# Ofborg will run:
# nix-build ./default.nix -A nixosTests.nginx
```

#### Re-evaluate

```bash
# Force re-evaluation (only if eval failed)
@ofborg eval
```

#### Multiple Commands

```bash
# Combine commands
@ofborg build hello @ofborg test hello

# Or on separate lines
@ofborg build firefox
@ofborg test firefox
```

### Interpreting Ofborg Results

**Green checkmarks (✓)**:
- Package built successfully
- Tests passed
- Evaluation succeeded

**Red X (✗)**:
- Build failed
- Tests failed
- Evaluation error

**Orange dot**:
- Skipped (marked as broken, unsupported platform)
- Not evaluated yet

Click "Details" to see:
- Build logs
- Error messages
- Evaluation output

### When to Use Ofborg Commands

**Use `@ofborg build`**:
- After fixing build failure
- To test on platforms you don't have access to
- To verify changes to dependencies

**Use `@ofborg test`**:
- After changing NixOS modules
- To verify service configurations
- After updating packages that tests depend on

**Use `@ofborg eval`**:
- Only when evaluation check fails
- After fixing syntax errors
- Usually not needed (auto-runs on every push)

## Triaging Issues

### Issue Categories

1. **Bug Reports** - Package broken or malfunctioning
2. **Version Requests** - Request to update to newer version
3. **Feature Requests** - New functionality or options
4. **Security** - CVE reports or vulnerabilities
5. **Documentation** - Unclear or missing docs

### Triage Process

#### 1. Verify the Issue

```bash
# Try to reproduce
nix-build -A package-name

# Check if already fixed
git log --oneline -- pkgs/path/to/package/

# Search for duplicate issues
# Use GitHub search: is:issue is:open package-name
```

#### 2. Label the Issue

Common labels:
- `0.kind: bug` - Something is broken
- `0.kind: enhancement` - Feature request
- `2.status: stale` - No activity for extended period
- `3.severity: security` - Security vulnerability
- `6.topic: *` - Topic-specific (python, darwin, etc.)
- `10.rebuild-*` - Impact on rebuilds

#### 3. Respond

**For valid bugs**:
```markdown
Thank you for the report. I can confirm this is a bug.

The issue is caused by [explanation].

I'll work on a fix. In the meantime, you can work around this by [workaround].
```

**For version requests**:
```markdown
Thanks for the request. I'll update this package to the latest version.

In the meantime, you can use the unstable channel or override the package:

\`\`\`nix
package-name.overrideAttrs (old: {
  version = "newversion";
  src = fetchurl {
    url = "...";
    hash = "...";
  };
})
\`\`\`
```

**For duplicates**:
```markdown
This is a duplicate of #12345. Please follow that issue for updates.
```

**For security issues**:
```markdown
Thank you for the security report. I'll prioritize fixing this.

Related CVE: CVE-2024-XXXXX

Tracking this with urgency.
```

#### 4. Take Action

- **Fix bugs**: Create PR with fix
- **Update versions**: Use nix-update and create PR
- **Close invalid**: Close with explanation
- **Request info**: Ask for more details if unclear

### Issue Best Practices

1. **Respond promptly** - Within a few days if possible
2. **Be respectful** - Kind even when closing invalid issues
3. **Provide context** - Explain why closing or deferring
4. **Link related work** - Reference related issues/PRs
5. **Use labels** - Help others find and categorize
6. **Request help** - Mention other maintainers if needed

## Reviewing Pull Requests

### PR Review Checklist

As a maintainer, review PRs touching your packages:

- [ ] **Builds successfully** - Check ofborg results
- [ ] **Tests pass** - All relevant tests green
- [ ] **Code quality** - Follows nixpkgs conventions
- [ ] **Commit message** - Clear and follows format
- [ ] **No regressions** - Doesn't break dependent packages
- [ ] **Documentation** - Updated if needed
- [ ] **Breaking changes** - Documented if applicable

### Review Comments

**Request changes**:
```markdown
Thanks for the PR! A few comments:

1. The commit message should follow the format: `package-name: 1.0 -> 2.0`
2. Please add yourself to `meta.maintainers`
3. The build is failing on darwin - see ofborg results

Could you address these? Thanks!
```

**Approve**:
```markdown
LGTM! Thank you for the contribution.

Builds successfully on x86_64-linux and tests pass.
```

**Request ofborg builds**:
```markdown
Looks good. Let's test on more platforms:

@ofborg build package-name
```

### Merging PRs

#### Using nixpkgs-merge-bot

For packages you maintain:

```markdown
@nixpkgs-merge-bot merge
```

Requirements:
- You're listed in meta.maintainers
- All required checks pass
- PR meets security constraints

#### Manual Merge

If merge-bot unavailable:

1. Ensure all checks pass
2. Click "Squash and merge" or "Rebase and merge"
3. Edit commit message if needed
4. Confirm merge

### Merge Conflicts

**Simple approach**: Ask contributor to rebase

```markdown
Could you please rebase on latest master? There are conflicts in the package definition.

\`\`\`bash
git fetch upstream
git rebase upstream/master
git push --force-with-lease
\`\`\`
```

**Complex conflicts**: Offer to help or take over

```markdown
These conflicts are complex. I can take over the PR if you'd like, or I can help resolve them. Let me know your preference!
```

## Keeping Packages Updated

### Monitoring Updates

#### Manual Checks

```bash
# Check latest release on GitHub
gh release list -R owner/repo

# Check package repository
curl -s https://api.github.com/repos/owner/repo/releases/latest | jq .tag_name
```

#### Automated Tools

**repology.org**: Tracks package versions across distros
- Visit https://repology.org/project/package-name/versions
- See if nixpkgs is behind

**GitHub watch**: Enable notifications for releases
- Go to package repository
- Click "Watch" → "Custom" → "Releases"

#### r-ryantm bot

Many packages are automatically updated by r-ryantm bot:
- Creates PRs for version bumps
- Updates hashes automatically
- Runs basic builds
- Maintainers just need to review and merge

### Update Workflow

```bash
# 1. Create update branch
git checkout -b update/package-name

# 2. Update with nix-update
nix-update --build --commit package-name

# 3. Test thoroughly
nix-build -A package-name
./result/bin/package-name --version

# 4. Review with nixpkgs-review
nixpkgs-review wip

# 5. Push and create PR
git push -u origin update/package-name
gh pr create --title "package-name: 1.0.0 -> 1.1.0"
```

### Handling Security Updates

**Priority**: Security updates should be handled urgently

```bash
# 1. Update immediately
nix-update --version=1.0.1 package-name

# 2. Build and test
nix-build -A package-name
nixpkgs-review wip

# 3. Commit with CVE reference
git commit -s -m "package-name: 1.0.0 -> 1.0.1 (security)

Fixes CVE-2024-XXXXX: Description of vulnerability

Security advisory: https://..."

# 4. Create PR with security label
gh pr create --label "3.severity: security"

# 5. Consider backporting to stable
# Mention in PR description if should be backported
```

## Common Maintainer Scenarios

### Scenario 1: Package Breaks After Update

```bash
# Someone else updated a dependency, your package breaks

# 1. Investigate
nix-build -A your-package --keep-failed
# Check error logs

# 2. Fix
# Option A: Update your package
nix-update --build your-package

# Option B: Add patch
vim pkgs/.../fix-new-dependency.patch

# Option C: Pin dependency temporarily
buildInputs = [ (dependency.overrideAttrs ...) ];

# 3. Create PR
git commit -s -m "your-package: fix build after dependency update"
```

### Scenario 2: User Reports Bug

```bash
# 1. Reproduce
nix-build -A package-name
./result/bin/package-name  # Try to trigger bug

# 2. Fix locally
vim pkgs/by-name/pa/package-name/package.nix
nix-build -A package-name
# Verify fix

# 3. Create PR
git commit -s -m "package-name: fix bug-description

Fixes #12345"

# 4. Comment on issue
# "Fixed in #12346. Will be in unstable soon."
```

### Scenario 3: Inactive Co-Maintainer

If co-maintainer doesn't respond for 3+ months:

```markdown
# In PR description
@inactive-maintainer hasn't responded to mentions in 3 months.

Per maintainer guidelines, proceeding with this change.

Removing from maintainers list as they appear inactive.
```

### Scenario 4: Breaking Change Needed

```markdown
# Create discussion issue first
**Title**: Proposal: Breaking change to package-name

## Motivation
[Why this change is needed]

## Breaking Changes
- Configuration format change
- API changes
- Migration path

## Timeline
Proposing for next release cycle.

## Migration Guide
[How users should update]

---

Tagging co-maintainers: @user1 @user2
```

## Maintainer Best Practices

1. **Respond timely** - Aim for response within a week
2. **Be kind** - Remember contributors are volunteers
3. **Document decisions** - Explain why in issues/PRs
4. **Test thoroughly** - Don't merge broken packages
5. **Welcome contributors** - Encourage and guide new contributors
6. **Ask for help** - Mention other maintainers when stuck
7. **Keep updated** - Regular version bumps prevent drift
8. **Security first** - Prioritize CVE fixes
9. **Communicate** - Update issues when working on fixes
10. **Share knowledge** - Help onboard new maintainers

## Maintainer Permissions

### What Maintainers Can Do

- Merge PRs for packages they maintain
- Use @nixpkgs-merge-bot for their packages
- Close/reopen issues for their packages
- Label issues related to their packages
- Make decisions about package direction
- Revert changes that break their packages

### What Maintainers Cannot Do

- Merge PRs for packages they don't maintain (without approval)
- Force merge without passing checks
- Ignore committer/community feedback
- Make changes that break other packages without fixing them

### Conflicts and Resolution

If disagreement with another maintainer:
1. Discuss in the issue/PR
2. Seek input from other maintainers
3. Bring to #nixpkgs-dev if needed
4. Maintainer of affected package has priority

## Stepping Down

If you can't maintain a package anymore:

```bash
# Remove yourself from maintainers list
vim pkgs/by-name/pa/package-name/package.nix

# In meta.maintainers, remove your handle

git commit -s -m "package-name: remove myself as maintainer

No longer have time to maintain this package.
Looking for new maintainer."
```

Optional: Create issue seeking new maintainer

```markdown
**Title**: Looking for new maintainer: package-name

I'm stepping down as maintainer of package-name due to time constraints.

Looking for someone interested in maintaining this package. Responsibilities include:
- Keeping up with updates
- Reviewing PRs
- Fixing bugs

Please comment if interested!
```

## Resources

- [Maintainer README](https://github.com/NixOS/nixpkgs/blob/master/maintainers/README.md)
- [Maintainer List](https://github.com/NixOS/nixpkgs/blob/master/maintainers/maintainer-list.nix)
- [Ofborg Documentation](https://github.com/NixOS/ofborg)
- [nixpkgs-merge-bot](https://github.com/NixOS/nixpkgs-merge-bot)
- [Reviewing Contributions](https://ryantm.github.io/nixpkgs/contributing/reviewing-contributions/)
