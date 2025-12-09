# Update Package Workflow

Update existing packages in NixOS/nixpkgs to newer versions.

## ⚠️ CRITICAL: Always Test With nixpkgs-review wip

**Before submitting your update PR, you MUST run `nixpkgs-review wip`:**

```bash
nixpkgs-review wip

# This verifies:
# ✓ Your updated package builds
# ✓ All dependent packages still work
# ✓ No regressions introduced
```

**This is not optional - CI will catch issues anyway, but wasting CI resources and reviewer time is disrespectful to the community.**

## When to Use

- "update nixpkgs package"
- "bump package version"
- "upgrade package to latest"
- "nix-update package"

## Quick Reference

### Automatic Update with nix-update

```bash
# 1. Create branch
git checkout -b update/package-name

# 2. Update package
nix-update package-name

# 3. Build and test
nix-build -A package-name

# 4. Review changes
nixpkgs-review wip

# 5. Commit and push
git add .
git commit -s -m "package-name: 1.0.0 -> 1.1.0"
git push -u origin update/package-name

# 6. Create PR
gh pr create
```

### Manual Update

```bash
# 1. Edit package file
vim pkgs/by-name/pa/package-name/package.nix

# 2. Update version
version = "1.1.0";  # was "1.0.0"

# 3. Update hash (use fake hash first)
hash = lib.fakeHash;

# 4. Build to get correct hash
nix-build -A package-name
# Copy correct hash from error

# 5. Test and commit
```

## Using nix-update

### Install nix-update

```bash
# Run without installing
nix run nixpkgs#nix-update

# Install to profile
nix profile install nixpkgs#nix-update

# In development shell
nix-shell -p nix-update
```

### Basic Update

```bash
# Update to latest version
nix-update package-name

# nix-update will:
# 1. Fetch latest version from upstream
# 2. Update version in package file
# 3. Update source hash
# 4. Update cargo/npm/vendor hashes if needed
```

### Update with Options

```bash
# Update and build
nix-update --build package-name

# Update and run tests
nix-update --test package-name

# Update and commit
nix-update --commit package-name

# Update, build, and commit
nix-update --build --commit package-name

# Update and format
nix-update --format package-name

# All together
nix-update --build --test --format --commit package-name
```

### Update to Specific Version

```bash
# Update to specific version
nix-update --version=1.2.3 package-name

# Update to unstable/latest
nix-update --version=unstable package-name

# Update to branch
nix-update --version=branch=main package-name
```

### Update Different Package Types

```bash
# Python package
nix-update python3Packages.package-name

# Go module
nix-update package-name

# Rust package
nix-update package-name

# Node package (if supported)
nix-update nodePackages.package-name
```

## Manual Update Process

### Step 1: Update Version

Edit the package file:

```nix
# Before
stdenv.mkDerivation rec {
  pname = "package-name";
  version = "1.0.0";
  # ...
}

# After
stdenv.mkDerivation rec {
  pname = "package-name";
  version = "1.1.0";
  # ...
}
```

### Step 2: Update Source Hash

#### Method 1: Fake Hash
```nix
src = fetchFromGitHub {
  owner = "owner";
  repo = "repo";
  rev = "v${version}";
  hash = lib.fakeHash;  # Temporary fake hash
};
```

Build to get correct hash:
```bash
nix-build -A package-name
```

Error will show correct hash:
```
error: hash mismatch in fixed-output derivation
  specified: sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
     got:    sha256-RealHashHere...
```

Copy the "got:" hash.

#### Method 2: nix-prefetch
```bash
# For GitHub
nix-prefetch-github owner repo --rev v1.1.0

# For tarball
nix-prefetch-url https://example.com/package-1.1.0.tar.gz

# For git
nix-prefetch-git https://git.example.com/repo.git --rev v1.1.0
```

### Step 3: Update Vendor Hash (Go/Rust)

#### Go Packages (vendorHash)

```nix
buildGoModule rec {
  # ...
  vendorHash = lib.fakeHash;  # Temporary
}
```

Build to get correct hash:
```bash
nix-build -A package-name
```

Copy vendor hash from error message.

#### Rust Packages (cargoHash)

```nix
rustPlatform.buildRustPackage rec {
  # ...
  cargoHash = lib.fakeHash;  # Temporary
}
```

Build to get correct hash.

### Step 4: Check for Breaking Changes

Before committing:

```bash
# Build package
nix-build -A package-name

# Test binary
./result/bin/package-name --version
./result/bin/package-name --help

# Check changelog
# Visit upstream repository and review CHANGELOG.md
```

### Step 5: Build Dependents

```bash
# Use nixpkgs-review to build packages that depend on this
nixpkgs-review wip

# Ensure no packages break due to update
```

## Review Changes

### Use nixpkgs-review

```bash
# Review your update
nixpkgs-review wip

# This will:
# 1. Build updated package
# 2. Build all packages that depend on it
# 3. Report any breakages
# 4. Drop you into nix-shell for testing

# In nix-shell
package-name --version  # Should show new version
package-name --help
exit
```

### Check Diff

```bash
# View changes
git diff

# Should see:
# - version = "1.0.0"; + version = "1.1.0";
# - hash = "sha256-old..."; + hash = "sha256-new...";
```

## Commit Message Format

### Simple Version Bump

```
package-name: 1.0.0 -> 1.1.0
```

### Version Bump with Details

```
package-name: 1.0.0 -> 1.1.0

Notable changes:
- Added new feature X
- Fixed CVE-2024-XXXXX
- Breaking: removed deprecated API Y

Changelog: https://github.com/owner/repo/releases/tag/v1.1.0
```

### Major Version Update

```
package-name: 1.0.0 -> 2.0.0

Breaking changes:
- API changed from X to Y
- Configuration format updated
- Minimum Go version now 1.21

Migration guide: https://...
```

### Security Update

```
package-name: 1.0.0 -> 1.0.1 (security)

Fixes CVE-2024-XXXXX: Remote code execution vulnerability

Security advisory: https://...
```

## Common Update Scenarios

### Routine Version Update

```bash
# Automated workflow
nix-update --build --commit package-name
nixpkgs-review wip
git push -u origin update/package-name
gh pr create
```

### Major Version Update

```bash
# More careful approach
nix-update --version=2.0.0 package-name
nix-build -A package-name
./result/bin/package-name  # Thorough testing
nixpkgs-review wip  # Check all dependents
# Review breaking changes
git commit -s -m "package-name: 1.0 -> 2.0 (breaking changes)"
```

### Security Update

```bash
# Quick turnaround
nix-update --version=1.0.1 package-name
nix-build -A package-name
nixpkgs-review wip
git commit -s -m "package-name: 1.0.0 -> 1.0.1 (CVE-2024-XXXXX)"
git push
gh pr create --title "package-name: security update" --label "security"
```

### Update with Dependency Changes

```bash
# Update package
nix-update package-name

# Edit package to add new dependencies
vim pkgs/by-name/pa/package-name/package.nix

# Add new buildInputs
buildInputs = [ oldDep newDep ];

# Build and test
nix-build -A package-name
nixpkgs-review wip

# Commit with dependency note
git commit -s -m "package-name: 1.0.0 -> 1.1.0

- Add newDep dependency for new feature
"
```

## Python Package Updates

### Update Python Package

```bash
# Python packages in pkgs/development/python-modules/
nix-update python3Packages.package-name

# Or manually
vim pkgs/development/python-modules/package-name/default.nix
```

### Update PyPI Hash

```bash
# Use nix-prefetch-url
nix-prefetch-url https://pypi.io/packages/source/p/package-name/package-name-1.1.0.tar.gz

# Or use fake hash
src = fetchPypi {
  inherit pname version;
  hash = lib.fakeHash;
};
```

## Handling Update Failures

### Build Fails After Update

```bash
# Keep build directory
nix-build -A package-name --keep-failed

# Inspect build
cd /tmp/nix-build-package-*
cat build.log

# Common issues:
# - New dependencies needed
# - Build system changed
# - Tests failing
```

### Add New Dependencies

```nix
# Add to nativeBuildInputs or buildInputs
buildInputs = [
  existingDep
  newlyRequiredDep  # Added in v1.1.0
];
```

### Disable Failing Tests

```nix
# Temporary workaround
doCheck = false;

# Or skip specific tests
checkPhase = ''
  runHook preCheck
  pytest -k "not failing_test_name"
  runHook postCheck
'';
```

### Apply Patches

```nix
# Add patch for nixpkgs-specific fixes
patches = [
  ./fix-build-on-nix.patch
];
```

## Checklist

Before submitting update PR:

- [ ] Package builds successfully
- [ ] Binary executes and shows new version
- [ ] Ran `nixpkgs-review wip`
- [ ] No packages break due to update
- [ ] Reviewed upstream changelog
- [ ] Updated hash correctly
- [ ] Updated vendorHash/cargoHash if needed
- [ ] Commit message follows convention
- [ ] Signed commit (`-s` flag)
- [ ] For major updates: documented breaking changes
- [ ] For security updates: referenced CVE

## Create Pull Request

```bash
# Push branch
git push -u origin update/package-name

# Create PR
gh pr create

# Title: package-name: 1.0.0 -> 1.1.0
# For security: add [security] label
```

### PR Description Template

```markdown
## Update Details
- Old version: 1.0.0
- New version: 1.1.0
- [Upstream changelog](https://github.com/owner/repo/releases/tag/v1.1.0)

## Testing
- [x] Builds successfully
- [x] Binary executes
- [x] `--version` shows 1.1.0
- [x] Ran nixpkgs-review (no breakages)

## Notable Changes
- Bug fixes and performance improvements
- No breaking changes
```

## Batch Updates

For updating multiple related packages:

```bash
# Update all python packages (example)
for pkg in package1 package2 package3; do
  nix-update python3Packages.$pkg --build
done

# Review all changes together
nixpkgs-review wip

# Commit message
git commit -s -m "python3Packages: update multiple packages

- package1: 1.0 -> 1.1
- package2: 2.0 -> 2.1
- package3: 3.0 -> 3.2
"
```

## Resources

- [nix-update](https://github.com/Mic92/nix-update)
- [nixpkgs-review](https://github.com/Mic92/nixpkgs-review)
- [Contributing to Nixpkgs](https://github.com/NixOS/nixpkgs/blob/master/CONTRIBUTING.md)
- [Package Updates](https://nixos.org/manual/nixpkgs/stable/#sec-package-updates)
