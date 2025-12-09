# Add Package Workflow

Add new packages to NixOS/nixpkgs repository.

## ⚠️ CRITICAL: Always Test With nixpkgs-review wip

**Before submitting your new package PR, you MUST run `nixpkgs-review wip`:**

```bash
nixpkgs-review wip

# This verifies:
# ✓ Your package builds successfully
# ✓ No evaluation errors
# ✓ Package can be instantiated
```

**Untested packages that don't build will be immediately rejected.**

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

### Basic Package Structure (Modern Pattern)

**Using finalAttrs** (recommended for new packages):

```nix
# pkgs/by-name/pa/package-name/package.nix
{
  lib,
  stdenv,
  fetchFromGitHub,
  # Add build dependencies here
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "package-name";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "owner";
    repo = "repo";
    rev = "v${finalAttrs.version}";
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  nativeBuildInputs = [
    # Build-time dependencies (compilers, build tools)
  ];

  buildInputs = [
    # Runtime dependencies (libraries)
  ];

  meta = {
    description = "Brief description of what this package does";
    homepage = "https://github.com/owner/repo";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ your-github-username ];
    platforms = lib.platforms.linux;
    mainProgram = "package-name";
  };
})
```

**Traditional rec pattern** (still acceptable):

```nix
stdenv.mkDerivation rec {
  pname = "package-name";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "owner";
    repo = "repo";
    rev = "v${version}";
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  # ... rest of package
}
```

**Important**: When using `finalAttrs`, reference version/pname within src as `finalAttrs.version` and `finalAttrs.pname`. Avoid using `with lib;` in meta - use explicit `lib.licenses`, `lib.platforms` instead for better overriding support.

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

## Modern Nix Patterns

### The finalAttrs Pattern

The `finalAttrs` pattern provides self-referencing within package definitions:

```nix
stdenv.mkDerivation (finalAttrs: {
  pname = "package";
  version = "1.0.0";

  # Can reference finalAttrs.version here
  src = fetchurl {
    url = "https://example.com/${finalAttrs.pname}-${finalAttrs.version}.tar.gz";
    hash = "...";
  };

  # Can also reference in build phases
  buildPhase = ''
    echo "Building ${finalAttrs.pname} version ${finalAttrs.version}"
  '';
})
```

**Benefits**:
- Better override support (`overrideAttrs` works correctly)
- No need for `rec { }` (avoids recursion issues)
- Clearer intent when self-referencing

**When to use**:
- Recommended for all new packages
- Especially when referencing pname/version in src, phases, or scripts
- Migration from `rec` is encouraged but not required

**When NOT to use finalAttrs.pname/version in src hash**:
```nix
# DON'T DO THIS - hash won't change when overriding
src = fetchurl {
  url = "https://example.com/${finalAttrs.pname}-${finalAttrs.version}.tar.gz";
  hash = "sha256-fixedhash...";  # This hash is for specific version!
};
```

Overriding version will cause hash mismatch. Instead, users must override both version AND hash when needed.

### Meta Attributes Without 'with lib'

Modern style avoids `with lib;` in meta for better overriding:

```nix
# Modern (recommended)
meta = {
  description = "Package description";
  license = lib.licenses.mit;
  maintainers = with lib.maintainers; [ yourhandle ];
  platforms = lib.platforms.linux;
};

# Old style (still works, but less preferred)
meta = with lib; {
  description = "Package description";
  license = licenses.mit;
  maintainers = with maintainers; [ yourhandle ];
  platforms = platforms.linux;
};
```

The `with lib.maintainers` is acceptable since it's a simple lookup list.

## Common Review Feedback

Expect reviewers to ask for these improvements:

### 1. "Please use pkgs/by-name structure"
```nix
# Move from:
pkgs/tools/networking/package-name/default.nix

# To:
pkgs/by-name/pa/package-name/package.nix
```

### 2. "Add yourself as maintainer"
```nix
meta = {
  maintainers = with lib.maintainers; [ yourhandle ];
};
```

### 3. "Set mainProgram"
```nix
meta = {
  mainProgram = "binary-name";  # The primary executable
};
```

### 4. "Use finalAttrs pattern"
```nix
# Change from:
stdenv.mkDerivation rec {

# To:
stdenv.mkDerivation (finalAttrs: {
```

### 5. "Format with nixfmt"
```bash
nixfmt pkgs/by-name/pa/package-name/package.nix
```

### 6. "Fix commit message format"
```bash
# Should be:
package-name: init at 1.0.0

# Not:
Add package-name
Added new package package-name
```

### 7. "Add package description"
```nix
meta = {
  description = "Brief, clear description of what this does";
  # Not: "A package for..."
  # Not: "package-name is a..."
};
```

### 8. "Specify correct license"
```nix
# Check upstream LICENSE file
meta = {
  license = lib.licenses.mit;  # Match upstream exactly
};
```

### 9. "Remove unnecessary dependencies"
```nix
# Only include dependencies actually used
# Reviewers may ask: "Is pkg-config actually needed?"
```

### 10. "Use nativeBuildInputs for build tools"
```nix
# Move build tools from buildInputs
nativeBuildInputs = [ cmake pkg-config ];
buildInputs = [ openssl ];  # Only runtime deps
```

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

## Real Package Examples

### Example 1: Simple CLI Tool (Go)

A minimal Go CLI tool with no extra dependencies:

```nix
# pkgs/by-name/he/hello-go/package.nix
{
  lib,
  buildGoModule,
  fetchFromGitHub,
}:

buildGoModule rec {
  pname = "hello-go";
  version = "1.2.3";

  src = fetchFromGitHub {
    owner = "example";
    repo = "hello-go";
    rev = "v${version}";
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  vendorHash = "sha256-BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB=";

  ldflags = [
    "-s"
    "-w"
    "-X main.version=${version}"
  ];

  meta = {
    description = "Simple hello world CLI tool";
    homepage = "https://github.com/example/hello-go";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ yourhandle ];
    mainProgram = "hello-go";
  };
}
```

### Example 2: Rust Application with Dependencies

A Rust application that needs system libraries:

```nix
# pkgs/by-name/my/my-rust-app/package.nix
{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  openssl,
  stdenv,
  darwin,
}:

rustPlatform.buildRustPackage rec {
  pname = "my-rust-app";
  version = "2.1.0";

  src = fetchFromGitHub {
    owner = "example";
    repo = "my-rust-app";
    rev = "v${version}";
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  cargoHash = "sha256-BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB=";

  nativeBuildInputs = [ pkg-config ];

  buildInputs =
    [ openssl ]
    ++ lib.optionals stdenv.hostPlatform.isDarwin [
      darwin.apple_sdk.frameworks.Security
      darwin.apple_sdk.frameworks.SystemConfiguration
    ];

  meta = {
    description = "Rust application for doing something useful";
    homepage = "https://github.com/example/my-rust-app";
    license = lib.licenses.asl20;
    maintainers = with lib.maintainers; [ yourhandle ];
    mainProgram = "my-rust-app";
  };
}
```

### Example 3: C/C++ Application with CMake

Traditional C++ application using CMake build system:

```nix
# pkgs/by-name/my/myapp/package.nix
{
  lib,
  stdenv,
  fetchFromGitHub,
  cmake,
  pkg-config,
  zlib,
  libpng,
  boost,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "myapp";
  version = "3.0.1";

  src = fetchFromGitHub {
    owner = "example";
    repo = "myapp";
    rev = "v${finalAttrs.version}";
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  nativeBuildInputs = [
    cmake
    pkg-config
  ];

  buildInputs = [
    zlib
    libpng
    boost
  ];

  cmakeFlags = [
    "-DENABLE_TESTS=OFF"
    "-DBUILD_SHARED_LIBS=ON"
  ];

  meta = {
    description = "C++ application for processing images";
    homepage = "https://github.com/example/myapp";
    license = lib.licenses.gpl3Only;
    maintainers = with lib.maintainers; [ yourhandle ];
    platforms = lib.platforms.unix;
    mainProgram = "myapp";
  };
})
```

### Example 4: Python Application

Python CLI tool installed as application (not library):

```nix
# pkgs/by-name/py/pytool/package.nix
{
  lib,
  python3Packages,
  fetchFromGitHub,
}:

python3Packages.buildPythonApplication rec {
  pname = "pytool";
  version = "1.5.0";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "example";
    repo = "pytool";
    rev = "v${version}";
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  build-system = with python3Packages; [
    setuptools
    wheel
  ];

  dependencies = with python3Packages; [
    click
    requests
    pyyaml
  ];

  nativeCheckInputs = with python3Packages; [
    pytestCheckHook
  ];

  pythonImportsCheck = [ "pytool" ];

  meta = {
    description = "Python tool for data processing";
    homepage = "https://github.com/example/pytool";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ yourhandle ];
    mainProgram = "pytool";
  };
}
```

### Example 5: Application with Wrapper

Application that needs runtime dependencies in PATH:

```nix
# pkgs/by-name/my/myshell/package.nix
{
  lib,
  stdenv,
  fetchFromGitHub,
  makeWrapper,
  bash,
  git,
  curl,
  jq,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "myshell";
  version = "0.5.0";

  src = fetchFromGitHub {
    owner = "example";
    repo = "myshell";
    rev = "v${finalAttrs.version}";
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp myshell.sh $out/bin/myshell
    chmod +x $out/bin/myshell

    # Wrap to add runtime dependencies to PATH
    wrapProgram $out/bin/myshell \
      --prefix PATH : ${
        lib.makeBinPath [
          bash
          git
          curl
          jq
        ]
      }

    runHook postInstall
  '';

  meta = {
    description = "Shell script wrapper for git operations";
    homepage = "https://github.com/example/myshell";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ yourhandle ];
    mainProgram = "myshell";
  };
})
```

### Example 6: Desktop Application

GUI application with desktop entry:

```nix
# pkgs/by-name/my/myapp-gui/package.nix
{
  lib,
  stdenv,
  fetchFromGitHub,
  cmake,
  pkg-config,
  qt6,
  wrapQtAppsHook,
  makeDesktopItem,
  copyDesktopItems,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "myapp-gui";
  version = "2.0.0";

  src = fetchFromGitHub {
    owner = "example";
    repo = "myapp-gui";
    rev = "v${finalAttrs.version}";
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  nativeBuildInputs = [
    cmake
    pkg-config
    wrapQtAppsHook
    copyDesktopItems
  ];

  buildInputs = [
    qt6.qtbase
    qt6.qtsvg
  ];

  desktopItems = [
    (makeDesktopItem {
      name = "myapp-gui";
      exec = "myapp-gui";
      icon = "myapp-gui";
      desktopName = "MyApp GUI";
      comment = "GUI application for MyApp";
      categories = [ "Utility" ];
    })
  ];

  postInstall = ''
    install -Dm644 resources/icon.png $out/share/icons/hicolor/256x256/apps/myapp-gui.png
  '';

  meta = {
    description = "Graphical user interface for MyApp";
    homepage = "https://github.com/example/myapp-gui";
    license = lib.licenses.gpl3Plus;
    maintainers = with lib.maintainers; [ yourhandle ];
    platforms = lib.platforms.linux;
    mainProgram = "myapp-gui";
  };
})
```

## Resources

- [Nixpkgs Manual - Quick Start](https://nixos.org/manual/nixpkgs/stable/#chap-quick-start)
- [pkgs/by-name README](https://github.com/NixOS/nixpkgs/blob/master/pkgs/by-name/README.md)
- [Package Naming](https://nixos.org/manual/nixpkgs/stable/#sec-package-naming)
- [Contributing to Nixpkgs](https://github.com/NixOS/nixpkgs/blob/master/CONTRIBUTING.md)
- [Nixpkgs finalAttrs Discussion](https://github.com/NixOS/nixpkgs/issues/315337)
- [Nixpkgs Overriding Guide](https://nixos.org/manual/nixpkgs/stable/#chap-overrides)
