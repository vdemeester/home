# Add Package Workflow

Add new packages to NixOS/nixpkgs repository.

## When to Use

- "add package to nixpkgs"
- "create new nixpkgs package"
- "package new application"
- "init package in nixpkgs"

## Quick Reference

### Complete Workflow
```bash
# 1. Create branch
git checkout -b pkg/package-name

# 2. Create package in pkgs/by-name
mkdir -p pkgs/by-name/pa/package-name
vim pkgs/by-name/pa/package-name/package.nix

# 3. Build and test
nix-build -A package-name
./result/bin/package-name

# 4. Review with nixpkgs-review
nixpkgs-review wip

# 5. Format code
nixfmt pkgs/by-name/pa/package-name/package.nix

# 6. Commit and push
git add .
git commit -s -m "package-name: init at 1.0.0"
git push -u origin pkg/package-name

# 7. Create PR
gh pr create
```

## Package Location

### Use pkgs/by-name (Preferred)

For top-level packages:

```
pkgs/by-name/
  pa/package-name/package.nix
```

**Rules:**
- Use first 2 letters of package name as directory prefix
- Package directory name = attribute name
- File must be named `package.nix`
- Automatically included (no all-packages.nix change needed)

**Examples:**
```
pkgs/by-name/he/hello/package.nix          # hello package
pkgs/by-name/fi/firefox/package.nix        # firefox package
pkgs/by-name/go/go/package.nix             # go package
pkgs/by-name/my/my-tool/package.nix        # my-tool package
```

### Traditional Structure

For packages in specialized sets or categories:

```
pkgs/
  applications/          # GUI applications
  development/           # Development tools
    libraries/           # Libraries
    tools/               # Build tools
  servers/               # Server software
  tools/                 # Command-line tools
    networking/          # Network tools
    system/              # System tools
```

**When to use:**
- Python packages: `pkgs/development/python-modules/`
- Perl modules: `pkgs/development/perl-modules/`
- Node packages: Generated via `node2nix`
- Haskell packages: Generated via `cabal2nix`

## Package Template

### Basic Package Structure

```nix
# pkgs/by-name/pa/package-name/package.nix
{
  lib,
  stdenv,
  fetchFromGitHub,
  # Add build dependencies here
}:

stdenv.mkDerivation rec {
  pname = "package-name";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "owner";
    repo = "repo";
    rev = "v${version}";
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  nativeBuildInputs = [
    # Build-time dependencies (compilers, build tools)
  ];

  buildInputs = [
    # Runtime dependencies (libraries)
  ];

  meta = with lib; {
    description = "Brief description of what this package does";
    homepage = "https://github.com/owner/repo";
    license = licenses.mit;
    maintainers = with maintainers; [ your-github-username ];
    platforms = platforms.linux;
    mainProgram = "package-name";
  };
}
```

## Language-Specific Builders

### Go Package (buildGoModule)

```nix
{
  lib,
  buildGoModule,
  fetchFromGitHub,
}:

buildGoModule rec {
  pname = "package-name";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "owner";
    repo = "repo";
    rev = "v${version}";
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  vendorHash = "sha256-BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB=";

  ldflags = [
    "-s"
    "-w"
    "-X main.version=${version}"
  ];

  meta = with lib; {
    description = "Description of Go package";
    homepage = "https://github.com/owner/repo";
    license = licenses.mit;
    maintainers = with maintainers; [ your-github-username ];
    mainProgram = "package-name";
  };
}
```

**Getting vendorHash:**
```bash
# Use fake hash first
vendorHash = lib.fakeHash;

# Build will fail and show correct hash
nix-build -A package-name
# Copy hash from error message
```

### Rust Package (rustPlatform)

```nix
{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage rec {
  pname = "package-name";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "owner";
    repo = "repo";
    rev = "v${version}";
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  cargoHash = "sha256-CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC=";

  meta = with lib; {
    description = "Description of Rust package";
    homepage = "https://github.com/owner/repo";
    license = licenses.mit;
    maintainers = with maintainers; [ your-github-username ];
    mainProgram = "package-name";
  };
}
```

### Python Package (buildPythonPackage)

**Note:** Python packages go in `pkgs/development/python-modules/`

```nix
{
  lib,
  buildPythonPackage,
  fetchPypi,
  setuptools,
  wheel,
  # Test dependencies
  pytestCheckHook,
}:

buildPythonPackage rec {
  pname = "package-name";
  version = "1.0.0";
  format = "pyproject";

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  nativeBuildInputs = [
    setuptools
    wheel
  ];

  propagatedBuildInputs = [
    # Python runtime dependencies
  ];

  nativeCheckInputs = [
    pytestCheckHook
  ];

  pythonImportsCheck = [ "package_name" ];

  meta = with lib; {
    description = "Description of Python package";
    homepage = "https://github.com/owner/repo";
    license = licenses.mit;
    maintainers = with maintainers; [ your-github-username ];
  };
}
```

### Node Package

Node packages are typically generated using `node2nix`. See nixpkgs documentation for details.

## Getting Source Hash

### From GitHub

```bash
# Using nix-prefetch-github
nix-prefetch-github owner repo --rev v1.0.0

# Manual with nix-prefetch-url
nix-prefetch-url --unpack https://github.com/owner/repo/archive/v1.0.0.tar.gz
```

### From URL

```bash
# Prefetch tarball
nix-prefetch-url https://example.com/package-1.0.0.tar.gz

# For git repositories
nix-prefetch-git https://git.example.com/repo.git --rev v1.0.0
```

### Using Fake Hash

```nix
src = fetchFromGitHub {
  owner = "owner";
  repo = "repo";
  rev = "v1.0.0";
  hash = lib.fakeHash;  # Use fake hash
};
```

Build will fail with correct hash:
```
error: hash mismatch in fixed-output derivation
  specified: sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=
     got:    sha256-RealHashGoesHere...
```

Copy the "got:" hash.

## Build and Test

### Build Package

```bash
# Build package
nix-build -A package-name

# Result symlink created
ls -l result/

# Test binary
./result/bin/package-name --version
./result/bin/package-name --help
```

### Test Installation

```bash
# Install to profile
nix-env -f . -iA package-name

# Or with nix profile
nix profile install .#package-name

# Test installed version
package-name --version
```

### Run Tests

```bash
# Tests run automatically during build if doCheck = true
nix-build -A package-name

# To debug test failures
nix-build -A package-name --keep-failed
cd /tmp/nix-build-*
# Examine test output
```

## Review with nixpkgs-review

```bash
# Review your changes
nixpkgs-review wip

# This will:
# 1. Build your new package
# 2. Build packages that depend on it
# 3. Drop you into nix-shell
# 4. Test the package

# In nix-shell
package-name --version
package-name --help

# Exit when satisfied
exit
```

## Format Code

```bash
# Format Nix file
nixfmt pkgs/by-name/pa/package-name/package.nix

# Or format all changed files
nixfmt $(git diff --name-only '*.nix')
```

## Commit Message Format

### New Package

```
package-name: init at 1.0.0
```

### New Package with Details

```
package-name: init at 1.0.0

Package description and why it's useful.

Closes #12345
```

### Python Package

```
python3Packages.package-name: init at 1.0.0
```

## Create Pull Request

### Push Branch

```bash
# Push to your fork
git push -u origin pkg/package-name
```

### Create PR with GitHub CLI

```bash
# Create PR
gh pr create

# Fill in:
# Title: package-name: init at 1.0.0
# Description: What the package does, why it's useful
```

### PR Description Template

```markdown
## Description
Brief description of what this package does.

## Checklist
- [ ] Built and tested locally
- [ ] Ran nixpkgs-review
- [ ] Formatted with nixfmt
- [ ] Meta attributes complete (description, license, maintainers)
- [ ] mainProgram set (if applicable)

## Testing
Tested on x86_64-linux:
- [x] Builds successfully
- [x] Binary executes
- [x] --help and --version work
```

## Checklist

Before submitting PR:

- [ ] Package builds successfully (`nix-build -A package-name`)
- [ ] Binary works (`./result/bin/package-name`)
- [ ] Ran `nixpkgs-review wip`
- [ ] Formatted with `nixfmt`
- [ ] Used `pkgs/by-name` structure (if top-level package)
- [ ] Correct package name (lowercase, hyphens)
- [ ] Accurate description
- [ ] Correct license
- [ ] Added yourself to maintainers
- [ ] Set `mainProgram` (if applicable)
- [ ] Commit message follows convention
- [ ] Signed commit (`-s` flag)

## Common Issues

### Package Name

- Use lowercase
- Use hyphens, not underscores
- Match upstream name when possible
- Don't include language prefix (no "python-", "go-", etc.)

### Dependencies

**nativeBuildInputs** (build-time):
- Compilers (gcc, rustc)
- Build tools (cmake, meson, pkg-config)
- Code generators

**buildInputs** (runtime):
- Libraries (openssl, zlib)
- Runtime dependencies

**propagatedBuildInputs** (dependencies that must be in runtime environment):
- Libraries that expose headers
- Python dependencies

### License

Find license in upstream repository:

```nix
# Common licenses
licenses.mit
licenses.asl20        # Apache 2.0
licenses.gpl3Only
licenses.lgpl3Only
licenses.bsd3
licenses.mpl20        # Mozilla Public License 2.0
```

### Platforms

```nix
# Common platform sets
platforms.linux
platforms.darwin
platforms.unix        # Linux + macOS
platforms.all
```

## Resources

- [Nixpkgs Manual - Quick Start](https://nixos.org/manual/nixpkgs/stable/#chap-quick-start)
- [pkgs/by-name README](https://github.com/NixOS/nixpkgs/blob/master/pkgs/by-name/README.md)
- [Package Naming](https://nixos.org/manual/nixpkgs/stable/#sec-package-naming)
- [Contributing to Nixpkgs](https://github.com/NixOS/nixpkgs/blob/master/CONTRIBUTING.md)
