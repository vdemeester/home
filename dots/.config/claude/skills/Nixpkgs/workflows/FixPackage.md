# Fix Package Workflow

Fix broken or failing packages in NixOS/nixpkgs.

## ⚠️ CRITICAL: Always Test With nixpkgs-review wip

**Before submitting your fix PR, you MUST run `nixpkgs-review wip`:**

```bash
nixpkgs-review wip

# This verifies:
# ✓ Your fix actually works
# ✓ Package builds successfully
# ✓ No new regressions in dependents
```

**Don't assume your fix works without testing - use nixpkgs-review!**

## When to Use

- "fix broken package"
- "package build failing"
- "fix nixpkgs package"
- "package doesn't work"

## Quick Reference

### Fix Workflow

```bash
# 1. Create branch
git checkout -b fix/package-name-issue

# 2. Identify the problem
nix-build -A package-name --keep-failed
cd /tmp/nix-build-*
# Examine error

# 3. Fix package
vim pkgs/by-name/pa/package-name/package.nix

# 4. Test fix
nix-build -A package-name
./result/bin/package-name

# 5. Review
nixpkgs-review wip

# 6. Commit
git add .
git commit -s -m "package-name: fix build on aarch64-linux"

# 7. Push and create PR
git push -u origin fix/package-name-issue
gh pr create
```

## Identifying Issues

### Check Build Status

```bash
# Try building
nix-build -A package-name

# Keep failed build directory
nix-build -A package-name --keep-failed

# View full build log
nix log --extra-experimental-features nix-command .#package-name
```

### Common Error Sources

1. **Build failures** - compilation errors, missing dependencies
2. **Test failures** - failing test suites
3. **Runtime failures** - package builds but doesn't execute
4. **Platform-specific** - works on x86_64 but not aarch64
5. **Hash mismatches** - upstream changed tarball
6. **Dependency conflicts** - incompatible with other packages

## Common Fixes

### Missing Dependencies

**Error:**
```
error: foo.h: No such file or directory
error: cannot find -lfoo
Package 'foo' not found
```

**Fix:**
```nix
# Add missing dependency
buildInputs = [
  existingDep
  foo  # Add the missing library
];

# For build tools
nativeBuildInputs = [
  pkg-config  # Often needed to find libraries
  cmake
];
```

### Wrong Dependency Type

```nix
# Build-time only (compilers, build tools)
nativeBuildInputs = [
  cmake
  pkg-config
  makeWrapper
];

# Runtime (libraries, executables)
buildInputs = [
  openssl
  zlib
  ncurses
];

# Libraries that expose headers
propagatedBuildInputs = [
  libfoo
];
```

### Test Failures

**Option 1: Disable all tests**
```nix
# Quick workaround
doCheck = false;
```

**Option 2: Skip specific failing tests**
```nix
checkPhase = ''
  runHook preCheck

  # Skip known failing test
  pytest -k "not test_that_fails"

  # Or for Go
  go test -skip="TestThatFails" ./...

  runHook postCheck
'';
```

**Option 3: Fix test environment**
```nix
preCheck = ''
  # Set up test environment
  export HOME=$TMPDIR
  export PATH=$PATH:${lib.makeBinPath [ git ]}
'';

nativeCheckInputs = [
  pytestCheckHook  # Python tests
  git  # Often needed by tests
];
```

### Platform-Specific Fixes

**Broken on specific platform:**
```nix
meta = with lib; {
  # Mark as broken on platform
  broken = stdenv.isDarwin;  # Broken on macOS
  broken = stdenv.isAarch64; # Broken on ARM

  # Or limit to specific platforms
  platforms = platforms.linux;  # Linux only
  platforms = [ "x86_64-linux" ];  # x86_64 Linux only
};
```

**Conditional dependencies:**
```nix
buildInputs = [
  commonDep
] ++ lib.optionals stdenv.isLinux [
  linuxOnlyDep
] ++ lib.optionals stdenv.isDarwin [
  darwinOnlyDep
];
```

### Hash Mismatch

**Error:**
```
error: hash mismatch in fixed-output derivation
  specified: sha256-AAAA...
     got:    sha256-BBBB...
```

**Fix:**
```nix
# Update to correct hash
src = fetchFromGitHub {
  owner = "owner";
  repo = "repo";
  rev = "v${version}";
  hash = "sha256-BBBB...";  # Use hash from error message
};
```

**If upstream changed tarball without version bump:**
```nix
# Document the issue
# Sometimes maintainers change release tarballs
# In this case, update hash and note in commit message
```

### Missing Files in Output

**Error:**
Package builds but files missing from `result/`

**Fix:**
```nix
installPhase = ''
  runHook preInstall

  # Ensure directories exist
  mkdir -p $out/bin
  mkdir -p $out/share/man/man1

  # Install files explicitly
  cp myapp $out/bin/
  cp myapp.1 $out/share/man/man1/

  runHook postInstall
'';
```

### Library Not Found at Runtime

**Error:**
```
error while loading shared libraries: libfoo.so.1: cannot open shared object file
```

**Fix:**
```nix
# Add autoPatchelfHook
nativeBuildInputs = [
  autoPatchelfHook
];

buildInputs = [
  libfoo  # Ensure library is in buildInputs
];

# Or manually patch
postInstall = ''
  patchelf --set-rpath ${lib.makeLibraryPath [ libfoo ]} $out/bin/myapp
'';
```

### Hardcoded Paths

**Error:**
```
/bin/sh: not found
/usr/bin/env: not found
```

**Fix:**
```nix
postPatch = ''
  # Replace hardcoded paths
  substituteInPlace script.sh \
    --replace '/bin/sh' '${stdenv.shell}' \
    --replace '/usr/bin/env' '${coreutils}/bin/env'

  # For Python scripts
  substituteInPlace script.py \
    --replace '/usr/bin/env python' '${python3}/bin/python3'
'';
```

## Applying Patches

### Create Patch File

```bash
# Make changes in package source
cd /tmp/nix-build-package-*/
# Edit files
git diff > fix-build.patch
cp fix-build.patch ~/nixpkgs/pkgs/by-name/pa/package-name/
```

### Apply Patch

```nix
{
  # ...
  patches = [
    ./fix-build.patch
    ./another-fix.patch
  ];
}
```

### Fetch Patch from URL

```nix
{
  patches = [
    # Fetch patch from upstream PR
    (fetchpatch {
      url = "https://github.com/owner/repo/commit/abc123.patch";
      hash = "sha256-...";
    })
  ];
}
```

## Advanced Fixes

### Override Build Phase

```nix
buildPhase = ''
  runHook preBuild

  # Custom build commands
  make -j$NIX_BUILD_CORES CUSTOM_FLAG=1

  runHook postBuild
'';
```

### Fix CMake/Meson Issues

```nix
# CMake
cmakeFlags = [
  "-DENABLE_TESTS=OFF"
  "-DUSE_SYSTEM_LIB=ON"
];

# Meson
mesonFlags = [
  "-Dtests=false"
  "-Dsystemd=true"
];
```

### Environment Variables

```nix
# Set environment for build
env = {
  NIX_CFLAGS_COMPILE = "-O2";
  GOFLAGS = "-tags=nogui";
};

# Or old style
NIX_CFLAGS_COMPILE = "-O2";
```

### Wrapper Scripts

```nix
postInstall = ''
  # Wrap binary to set environment
  wrapProgram $out/bin/myapp \
    --set CONFIG_DIR "$out/share/config" \
    --prefix PATH : ${lib.makeBinPath [ dependency ]}
'';

nativeBuildInputs = [ makeWrapper ];
```

## Testing Fixes

### Build Package

```bash
# Build fixed package
nix-build -A package-name

# Check outputs
ls -R result/

# Test binary
./result/bin/package-name --version
./result/bin/package-name --help
```

### Test on Multiple Platforms

```bash
# Build for aarch64
nix-build -A package-name --system aarch64-linux

# Or use nixpkgs-review with system flag
nixpkgs-review wip --system x86_64-linux,aarch64-linux
```

### Run Tests

```bash
# Enable tests
nix-build -A package-name --arg doCheck true

# Or temporarily in package
doCheck = true;
```

### Check Dependents

```bash
# Ensure fix doesn't break dependents
nixpkgs-review wip

# This builds all packages that depend on the fixed package
```

## Commit Message Format

### Simple Fix

```
package-name: fix build on aarch64-linux
```

### Fix with Details

```
package-name: fix build failure

The build was failing due to missing libfoo dependency.
Added libfoo to buildInputs to fix compilation.

Fixes #12345
```

### Platform-Specific Fix

```
package-name: fix build on darwin

- Add CoreFoundation to buildInputs
- Disable failing tests on macOS
- Patch hardcoded /bin/sh paths
```

### Test Fix

```
package-name: disable failing tests

Tests fail in sandbox environment due to network access.
Disabled network-dependent tests while keeping others.

See: https://github.com/owner/repo/issues/123
```

## Creating Pull Request

```bash
# Push fix
git push -u origin fix/package-name-issue

# Create PR
gh pr create

# Title: package-name: fix build on aarch64-linux
# Reference issue if applicable
```

### PR Description Template

```markdown
## Problem
Package fails to build on aarch64-linux with error:
```
error message here
```

## Solution
Added missing `libfoo` dependency to `buildInputs`.

## Testing
- [x] Builds successfully on x86_64-linux
- [x] Builds successfully on aarch64-linux
- [x] Binary executes
- [x] Tests pass
- [x] Ran nixpkgs-review (no breakages)

Fixes #12345
```

## Debugging Tips

### View Full Build Output

```bash
# More verbose
nix-build -A package-name -v

# Keep failed build
nix-build -A package-name --keep-failed

# Show trace
nix-build -A package-name --show-trace
```

### Inspect Build Environment

```bash
# Enter build environment
nix-shell '<nixpkgs>' -A package-name

# Now you can:
unpackPhase
cd $sourceRoot
ls -la
configurePhase
buildPhase
# etc.
```

### Check Dependencies

```bash
# Runtime dependencies
ldd result/bin/program

# Store path dependencies
nix-store --query --references result/

# Full dependency tree
nix-store --query --tree result/
```

### Search for Similar Packages

```bash
# Find similar packages that might have solutions
grep -r "similar-package" pkgs/

# Look at how other packages handle the issue
```

## Common Patterns

### Python Package Fixes

```nix
{
  # Disable import check if failing
  pythonImportsCheck = [ ];

  # Or fix imports
  pythonImportsCheck = [ "module_name" ];

  # Add missing Python dependencies
  propagatedBuildInputs = [
    requests
    click
  ];

  # Fix tests
  nativeCheckInputs = [
    pytestCheckHook
  ];

  # Skip specific tests
  disabledTests = [
    "test_network"  # Requires network
    "test_failing"  # Known failure
  ];
}
```

### Go Package Fixes

```nix
{
  # Update vendor hash
  vendorHash = "sha256-...";

  # Exclude vendor directory
  excludedPackages = [ "vendor" ];

  # Skip failing tests
  checkFlags = [
    "-skip=TestThatFails"
  ];

  # Set build tags
  tags = [ "netgo" ];
}
```

### Rust Package Fixes

```nix
{
  # Update cargo hash
  cargoHash = "sha256-...";

  # Disable default features
  buildNoDefaultFeatures = true;

  # Enable specific features
  buildFeatures = [ "feature1" "feature2" ];

  # Skip tests
  doCheck = false;
}
```

## Resources

- [Nixpkgs Manual - Debugging](https://nixos.org/manual/nixpkgs/stable/#chap-debugging)
- [NixOS Wiki - Troubleshooting](https://nixos.wiki/wiki/Troubleshooting)
- [Common Build Issues](https://nixos.org/manual/nixpkgs/stable/#chap-common-issues)
