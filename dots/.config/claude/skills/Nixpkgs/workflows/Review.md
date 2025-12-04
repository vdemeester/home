# Review Workflow

Review pull requests in NixOS/nixpkgs using nixpkgs-review.

## When to Use

- "review nixpkgs pr"
- "nixpkgs-review pr"
- "review pull request"
- "test nixpkgs pr"

## Quick Commands

### Basic PR Review
```bash
# Review PR by number
nixpkgs-review pr 12345

# Review and post results to PR
nixpkgs-review pr 12345 --post-result

# Review specific packages only
nixpkgs-review pr 12345 -p package-name
```

### Review Local Changes
```bash
# Review uncommitted changes
nixpkgs-review wip

# Review staged changes
nixpkgs-review wip --staged

# Review specific commit
nixpkgs-review rev HEAD
```

## Setup

### Install nixpkgs-review
```bash
# Run without installing
nix run nixpkgs#nixpkgs-review

# Install to profile
nix profile install nixpkgs#nixpkgs-review

# In development shell
nix-shell -p nixpkgs-review
```

### GitHub Authentication
Required for posting results with `--post-result`:

```bash
# Method 1: GitHub CLI (recommended)
gh auth login

# Method 2: Environment variable
export GITHUB_TOKEN=ghp_...

# Method 3: hub configuration (~/.config/hub)
github.com:
- user: username
  oauth_token: token
  protocol: https
```

## Review Workflow

### Step 1: Fetch and Review PR
```bash
# Review PR
nixpkgs-review pr 12345

# This will:
# 1. Fetch the PR
# 2. Determine changed packages
# 3. Build all changed packages
# 4. Drop you into nix-shell with built packages
```

### Step 2: Test Built Packages
In the nix-shell environment:

```bash
# Test the package works
package-name --version
package-name --help

# Check binary
which package-name
ls -la $(which package-name)

# For GUI applications
package-name &

# Check package structure
nix build .#package-name
tree result/
```

### Step 3: Review Changes
While in nix-shell, examine the PR:

```bash
# View PR comments
nixpkgs-review comments

# Check file changes
git diff master...pr-branch

# Review package definition
cat pkgs/path/to/package/default.nix
```

### Step 4: Approve or Request Changes
```bash
# In nix-shell: Approve if good
nixpkgs-review approve

# Exit shell
exit

# Or post results without approval
nixpkgs-review pr 12345 --post-result
```

## What to Check

### 1. Package Builds Successfully
- No build errors
- All outputs created
- Dependencies resolved

### 2. Tests Pass
```bash
# Check if tests are enabled
nix show-derivation .#package-name | grep doCheck

# Tests run during build
# Watch for test failures in build output
```

### 3. Binary Works
```bash
# Can execute
package-name --version

# Shows help
package-name --help

# Runs without errors
package-name (basic functionality test)
```

### 4. Dependencies Correct
```bash
# Check runtime dependencies
ldd result/bin/package-name

# Should not have missing libraries
# Should use Nix store paths
```

### 5. Meta Attributes
Check package metadata:

```nix
# In package definition
meta = with lib; {
  description = "Clear, accurate description";
  homepage = "https://correct-url.com";
  license = licenses.mit;  # Correct license
  maintainers = with maintainers; [ username ];
  platforms = platforms.linux;  # Appropriate platforms
};
```

### 6. No Regressions
```bash
# Check dependent packages still build
nixpkgs-review pr 12345  # Builds dependents automatically

# Look for failed dependents in output
```

### 7. Code Quality
- Follows Nix coding conventions
- Uses appropriate builders (buildGoModule, rustPlatform, etc.)
- No hardcoded paths
- Proper use of fetchFromGitHub/fetchurl
- Hash is correct (sha256/sha512)

## Review Scenarios

### Version Update PR
```bash
# Review version bump
nixpkgs-review pr 12345

# Check:
# - Version number updated correctly
# - Hash updated correctly
# - Tests still pass
# - No breaking changes
# - Changelog reviewed (if major update)
```

What to verify:
- Version matches upstream release
- Hash is correct (not placeholder)
- Dependencies still compatible
- No new runtime dependencies added without documentation

### New Package PR
```bash
# Review new package
nixpkgs-review pr 12345 -p new-package

# Check:
# - Package name follows conventions
# - In correct location (pkgs/by-name/ preferred)
# - Meta attributes complete
# - License specified correctly
# - Maintainers added
# - Tests included (if applicable)
# - Description accurate
```

Package location rules:
- Prefer `pkgs/by-name/xx/package-name/package.nix`
- Two-letter prefix from package name
- Must be top-level package
- Cannot use specialized callPackage (python3Packages, etc.)

### Security Update PR
```bash
# Review security update
nixpkgs-review pr 12345

# Check:
# - CVE mentioned in PR description
# - Version fixes vulnerability
# - All variants updated (if multiple)
# - Security advisory linked
# - Consider backport to stable
```

### Breaking Change PR
```bash
# Review breaking change
nixpkgs-review pr 12345

# Check:
# - Breaking changes documented
# - Migration guide provided
# - Dependents tested
# - Release notes updated
# - Staged appropriately (staging branch)
```

## Advanced Options

### Review Specific Packages
```bash
# Build only specific packages
nixpkgs-review pr 12345 -p firefox chromium

# Build packages matching regex
nixpkgs-review pr 12345 --package-regex "python.*"
```

### Cross-System Review
```bash
# Review for multiple systems
nixpkgs-review pr 12345 --system x86_64-linux,aarch64-linux
```

### Non-Interactive Review
```bash
# Don't enter shell
nixpkgs-review pr 12345 --no-shell

# Run custom command
nixpkgs-review pr 12345 --run "nix-shell -p hello --run hello"

# Print results to stdout
nixpkgs-review pr 12345 --print-result
```

### Sandbox Mode
```bash
# Protect HOME directory
nixpkgs-review pr 12345 --sandbox
```

## Posting Results

### Automatic Posting
```bash
# Post build report as PR comment
nixpkgs-review pr 12345 --post-result

# Requires GitHub authentication (see Setup)
```

The posted comment includes:
- Build status (success/failure)
- List of built packages
- Build logs (if failed)
- System information

### Manual Comments
If `--post-result` doesn't work:

1. Note which packages built successfully
2. Note any failures with error messages
3. Post manual comment on PR with findings
4. Include system (x86_64-linux, aarch64-linux, etc.)

## Review Best Practices

1. **Be constructive** - Focus on helping improve the PR
2. **Test thoroughly** - Don't just check if it builds
3. **Check license** - Ensure license matches upstream
4. **Verify platforms** - Check claimed platforms are correct
5. **Run the binary** - Actually execute the program
6. **Check description** - Should be clear and accurate
7. **Review commit message** - Should follow conventions
8. **Check for TODOs** - Ensure no placeholder comments
9. **Approve quickly** - Don't block good PRs unnecessarily
10. **Be respectful** - Remember there's a human behind the PR

## Common Issues

### Build Failures
```bash
# Keep build directory on failure
nixpkgs-review pr 12345 --keep-going

# View build logs
nix log .#package-name

# Show full trace
nixpkgs-review pr 12345 --show-trace
```

### Hash Mismatches
Common in updates:

```bash
# Correct hash shown in error
# Copy the "got:" hash to package definition
hash = "sha256-AAAA...";  # Wrong
hash = "sha256-BBBB...";  # Correct (from error)
```

### Missing Dependencies
```bash
# Check what's missing
ldd result/bin/program

# Add to buildInputs or nativeBuildInputs
nativeBuildInputs = [ pkg-config ];
buildInputs = [ libfoo ];
```

### Test Failures
```bash
# If tests fail but package works
# Suggest disabling tests with comment:
doCheck = false;

# Or skip specific tests
checkPhase = ''
  runHook preCheck
  pytest -k "not failing_test"
  runHook postCheck
'';
```

## Batch Review

For reviewing multiple PRs:

```bash
# Review multiple PRs
for pr in 12345 12346 12347; do
  nixpkgs-review pr $pr --no-shell --post-result
done

# Or in parallel (careful with resources!)
parallel nixpkgs-review pr {} --no-shell --post-result ::: 12345 12346 12347
```

## Resources

- [nixpkgs-review](https://github.com/Mic92/nixpkgs-review)
- [Reviewing Contributions](https://ryantm.github.io/nixpkgs/contributing/reviewing-contributions/)
- [Nixpkgs Manual - Contributing](https://nixos.org/manual/nixpkgs/stable/#chap-contributing)
