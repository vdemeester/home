# Package Workflow

Create, maintain, and debug Nix packages.

## When to Use

- "create nix package"
- "package application with nix"
- "write nix derivation"
- "buildGoModule"

## Quick Commands

### Building Packages
```bash
# Build package
nix build .#package-name

# Test package
nix build .#package-name -L

# Run package
nix run .#package-name

# Install package
nix profile install .#package-name
```

### Testing Packages
```bash
# Build and test
nix build .#package-name --rebuild

# Check meta attributes
nix eval .#package-name.meta --json

# Check outputs
nix eval .#package-name.outputs
```

## Package Structure

### Basic Package Template
```nix
# pkgs/mypackage/default.nix
{ lib
, stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation rec {
  pname = "mypackage";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "owner";
    repo = "repo";
    rev = "v${version}";
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  };

  meta = with lib; {
    description = "Short package description";
    homepage = "https://github.com/owner/repo";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    platforms = platforms.all;
  };
}
```

### Using callPackage
```nix
# pkgs/default.nix
{ pkgs }:
{
  mypackage = pkgs.callPackage ./mypackage { };
  anotherpkg = pkgs.callPackage ./anotherpkg { };
}
```

## Language-Specific Packaging

### Go Packages
```nix
{ lib
, buildGoModule
, fetchFromGitHub
}:

buildGoModule rec {
  pname = "mygoapp";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "owner";
    repo = "repo";
    rev = "v${version}";
    hash = "sha256-...";
  };

  vendorHash = "sha256-...";
  # Or if no vendor directory:
  # vendorHash = null;

  # Flags passed to go build
  ldflags = [
    "-s"
    "-w"
    "-X main.version=${version}"
  ];

  # Subdirectory with main package
  subPackages = [ "cmd/myapp" ];

  meta = with lib; {
    description = "My Go application";
    homepage = "https://github.com/owner/repo";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
```

#### Getting vendorHash
```bash
# Method 1: Use fake hash
# Set: vendorHash = lib.fakeSha256;
# Build will fail with correct hash

# Method 2: Use nix-prefetch
nix-prefetch -f '<nixpkgs>' '{
  mygoapp = (import ./. {}).mygoapp.go-modules;
}'

# Method 3: Build and copy error
nix build .#mygoapp 2>&1 | grep "got:"
```

### Rust Packages
```nix
{ lib
, rustPlatform
, fetchFromGitHub
, pkg-config
, openssl
}:

rustPlatform.buildRustPackage rec {
  pname = "myrustapp";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "owner";
    repo = "repo";
    rev = "v${version}";
    hash = "sha256-...";
  };

  cargoHash = "sha256-...";

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ openssl ];

  meta = with lib; {
    description = "My Rust application";
    homepage = "https://github.com/owner/repo";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
```

### Python Packages
```nix
{ lib
, python3Packages
, fetchPypi
}:

python3Packages.buildPythonPackage rec {
  pname = "mypythonpkg";
  version = "1.0.0";

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-...";
  };

  propagatedBuildInputs = with python3Packages; [
    requests
    flask
  ];

  checkInputs = with python3Packages; [
    pytest
  ];

  pythonImportsCheck = [ "mypythonpkg" ];

  meta = with lib; {
    description = "My Python package";
    homepage = "https://github.com/owner/repo";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
```

### Node.js Packages
```nix
{ lib
, buildNpmPackage
, fetchFromGitHub
}:

buildNpmPackage rec {
  pname = "mynodeapp";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "owner";
    repo = "repo";
    rev = "v${version}";
    hash = "sha256-...";
  };

  npmDepsHash = "sha256-...";

  # Skip npm install phase if needed
  dontNpmBuild = true;

  meta = with lib; {
    description = "My Node.js application";
    homepage = "https://github.com/owner/repo";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
```

## Source Fetchers

### fetchFromGitHub
```nix
src = fetchFromGitHub {
  owner = "owner";
  repo = "repo";
  rev = "v${version}";  # Or specific commit
  hash = "sha256-...";
};

# Get hash with:
nix-prefetch-github owner repo --rev v1.0.0
```

### fetchurl
```nix
src = fetchurl {
  url = "https://example.com/file-${version}.tar.gz";
  hash = "sha256-...";
};

# Get hash with:
nix-prefetch-url https://example.com/file.tar.gz
```

### fetchgit
```nix
src = fetchgit {
  url = "https://git.example.com/repo.git";
  rev = "commit-hash";
  hash = "sha256-...";
};
```

### fetchzip
```nix
src = fetchzip {
  url = "https://example.com/archive.zip";
  hash = "sha256-...";
  stripRoot = false;  # If archive doesn't have single root directory
};
```

## Build Phases

### Standard Phases
```nix
stdenv.mkDerivation {
  # ...

  # Phases run in order:
  # unpackPhase    - Extract source
  # patchPhase     - Apply patches
  # configurePhase - Run ./configure
  # buildPhase     - Run make
  # checkPhase     - Run tests
  # installPhase   - Install files
  # fixupPhase     - Fix up outputs

  # Override phases
  buildPhase = ''
    make all
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp myapp $out/bin/
  '';
}
```

### Custom Phases
```nix
stdenv.mkDerivation {
  # ...

  preBuild = ''
    echo "Before build"
  '';

  postInstall = ''
    echo "After install"
    # Wrap binary with runtime dependencies
    wrapProgram $out/bin/myapp \
      --prefix PATH : ${lib.makeBinPath [ dependency ]}
  '';
}
```

### Skip Phases
```nix
stdenv.mkDerivation {
  # ...

  # Skip configure phase
  dontConfigure = true;

  # Skip build phase
  dontBuild = true;

  # Skip tests
  doCheck = false;
}
```

## Dependencies

### Types of Dependencies
```nix
stdenv.mkDerivation {
  # Build-time dependencies (native to build platform)
  nativeBuildInputs = [
    pkg-config
    cmake
    makeWrapper
  ];

  # Runtime dependencies (target platform)
  buildInputs = [
    openssl
    zlib
  ];

  # Dependencies for running tests
  checkInputs = [
    pytest
  ];

  # Propagated to things that depend on this
  propagatedBuildInputs = [
    libxml2
  ];
}
```

### Finding Dependencies
```bash
# Search for package
nix search nixpkgs openssl

# Check package contents
nix-locate bin/openssl

# Find library
nix-locate --top-level lib/libssl.so
```

## Multiple Outputs

### Define Outputs
```nix
stdenv.mkDerivation {
  pname = "myapp";
  version = "1.0.0";

  outputs = [ "out" "dev" "doc" ];

  installPhase = ''
    # Main output
    mkdir -p $out/bin
    cp myapp $out/bin/

    # Development files
    mkdir -p $dev/include
    cp *.h $dev/include/

    # Documentation
    mkdir -p $doc/share/doc
    cp README.md $doc/share/doc/
  '';
}
```

### Use Specific Output
```bash
# Build specific output
nix build .#package.dev

# Install specific output
nix profile install .#package.doc
```

## Overriding Packages

### Override Attributes
```nix
# Override specific attributes
mypackage = pkgs.mypackage.overrideAttrs (old: {
  version = "2.0.0";
  src = fetchFromGitHub {
    # new source
  };
});
```

### Override Arguments
```nix
# Override function arguments
mypackage = pkgs.mypackage.override {
  enableFeature = true;
};
```

### Add Patches
```nix
mypackage = pkgs.mypackage.overrideAttrs (old: {
  patches = (old.patches or []) ++ [
    ./fix-bug.patch
  ];
});
```

## Meta Attributes

### Complete Meta Example
```nix
meta = with lib; {
  description = "Short one-line description";
  longDescription = ''
    Longer multi-line description
    explaining the package.
  '';
  homepage = "https://example.com";
  changelog = "https://example.com/changelog";
  license = licenses.mit;
  # Or multiple licenses
  # license = with licenses; [ mit asl20 ];
  maintainers = with maintainers; [ username ];
  platforms = platforms.linux;
  # Or specific platforms
  # platforms = [ "x86_64-linux" "aarch64-linux" ];
  broken = false;
  # Mark as unfree if needed
  # unfree = true;
};
```

### Common Licenses
```nix
licenses.mit
licenses.gpl3
licenses.gpl3Only
licenses.gpl3Plus
licenses.asl20
licenses.bsd3
licenses.mpl20
licenses.unfree
```

## Testing Packages

### Build and Test
```bash
# Build package
nix build .#mypackage -L

# Run tests
nix build .#mypackage --rebuild

# Check specific test
nix build .#mypackage.tests.sometest
```

### Test Installation
```bash
# Install to temporary profile
nix profile install .#mypackage --profile /tmp/test-profile

# Test binary
/tmp/test-profile/bin/myapp --version

# Clean up
rm -rf /tmp/test-profile
```

### Check Dependencies
```bash
# List runtime dependencies
nix-store -q --references $(nix build .#mypackage --no-link --print-out-paths)

# List all dependencies (recursive)
nix-store -q --requisites $(nix build .#mypackage --no-link --print-out-paths)

# Check for unwanted dependencies
nix-store -q --tree result | grep unwanted
```

## Debugging Packages

### Build Failures
```bash
# Keep build directory on failure
nix build .#mypackage --keep-failed

# Inspect failed build
cd /tmp/nix-build-mypackage-*.drv-0/
ls -la
cat build.log
```

### Enter Build Environment
```bash
# Enter environment
nix develop .#mypackage

# Run build phases manually
unpackPhase
cd $sourceRoot
configurePhase
buildPhase
```

### Check Build Script
```bash
# Show derivation
nix show-derivation .#mypackage

# Show builder
nix show-derivation .#mypackage | jq '.[].env.builder'

# Show build args
nix show-derivation .#mypackage | jq '.[].args'
```

## Package Best Practices

1. **Pin versions explicitly**: Use specific version numbers
2. **Add meta attributes**: Description, homepage, license, platforms
3. **Use appropriate fetcher**: fetchFromGitHub, fetchurl, etc.
4. **Include tests**: Enable doCheck when tests exist
5. **Multiple outputs**: Separate dev files, docs, etc.
6. **Minimal dependencies**: Only include what's needed
7. **Wrap binaries properly**: Use makeWrapper for runtime deps
8. **Document patches**: Comment why patches are needed
9. **Test cross-platform**: Build for different platforms
10. **Follow nixpkgs conventions**: Look at similar packages

## Common Patterns

### Wrapper Script
```nix
{ lib, stdenv, makeWrapper, myapp, dependency }:

stdenv.mkDerivation {
  pname = "myapp-wrapped";
  version = myapp.version;

  nativeBuildInputs = [ makeWrapper ];

  buildCommand = ''
    makeWrapper ${myapp}/bin/myapp $out/bin/myapp \
      --prefix PATH : ${lib.makeBinPath [ dependency ]}
  '';
}
```

### Conditional Features
```nix
{ lib
, stdenv
, enableFeature ? false
, optionalDependency
}:

stdenv.mkDerivation {
  pname = "myapp";

  buildInputs = [ ]
    ++ lib.optional enableFeature optionalDependency;

  configureFlags = [ ]
    ++ lib.optional enableFeature "--enable-feature";
}
```

### Platform-Specific
```nix
{ lib, stdenv, darwin }:

stdenv.mkDerivation {
  pname = "myapp";

  buildInputs = [ ]
    ++ lib.optionals stdenv.isLinux [ linuxDep ]
    ++ lib.optionals stdenv.isDarwin [
      darwin.apple_sdk.frameworks.Security
    ];
}
```

## Resources

- [Nixpkgs Manual - Stdenv](https://nixos.org/manual/nixpkgs/stable/#chap-stdenv)
- [Nixpkgs Manual - Languages](https://nixos.org/manual/nixpkgs/stable/#chap-language-support)
- [Nixpkgs Manual - Fetchers](https://nixos.org/manual/nixpkgs/stable/#chap-pkgs-fetchers)
- [Package Tutorials](https://nixos.wiki/wiki/Packaging)
