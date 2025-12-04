# Build Workflow

Build Nix packages, NixOS configurations, and understand the build process.

## When to Use

- "build nix package"
- "build nixos configuration"
- "nix build"
- "compile nix expression"

## Quick Commands

### Building Packages
```bash
# Build package from flake
nix build .#package-name

# Build and create result symlink
nix build .#package-name -o result

# Build specific output
nix build .#package-name.out

# Build without creating result symlink
nix build .#package-name --no-link

# Show build logs
nix build .#package-name -L
nix build .#package-name --print-build-logs
```

### Building NixOS Configurations
```bash
# Build NixOS configuration
nixos-rebuild build --flake .#hostname

# Build without switching
nix build .#nixosConfigurations.hostname.config.system.build.toplevel

# Dry build (show what would change)
nixos-rebuild dry-build --flake .#hostname

# Build with verbose output
nixos-rebuild build --flake .#hostname --show-trace
```

### Building for Different Systems
```bash
# Build for different architecture
nix build .#package-name --system x86_64-linux
nix build .#package-name --system aarch64-linux

# Cross-compile
nix build .#package-name.pkgsCross.aarch64-multiplatform
```

## Understanding Build Process

### Build Phases
```
Nix build phases (for stdenv.mkDerivation):
1. unpackPhase    - Extract source
2. patchPhase     - Apply patches
3. configurePhase - Run ./configure
4. buildPhase     - Run make
5. checkPhase     - Run tests
6. installPhase   - Install to output
7. fixupPhase     - Fix up outputs
```

### Viewing Build Steps
```bash
# Show derivation
nix show-derivation .#package-name

# Show build script
nix develop .#package-name --command cat $(type -p genericBuild)

# Build with debug output
nix build .#package-name --debug
```

## Building Packages

### From Flake
```bash
# Build package
nix build .#mypackage

# Build and run
nix run .#mypackage

# Build all packages
nix build .#packages.x86_64-linux.mypackage
```

### From nixpkgs
```bash
# Build package from nixpkgs
nix build nixpkgs#firefox

# Build specific version
nix build github:nixos/nixpkgs/nixos-23.11#firefox
```

### Build Local Package
```bash
# Build package in current directory
nix build

# Build with specific expression
nix build -f default.nix

# Build attribute
nix build -f . mypackage
```

## Building NixOS Systems

### Local System
```bash
# Build current system configuration
sudo nixos-rebuild build --flake .

# Build specific host
nixos-rebuild build --flake .#hostname

# Build and show what will change
nixos-rebuild dry-build --flake .#hostname
```

### Remote System
```bash
# Build for remote host
nixos-rebuild build --flake .#remote-host \
  --target-host user@remote-host \
  --use-remote-sudo

# Build locally for remote deployment
nix build .#nixosConfigurations.remote-host.config.system.build.toplevel
```

### Build VM
```bash
# Build VM from configuration
nixos-rebuild build-vm --flake .#hostname

# Run the VM
./result/bin/run-*-vm
```

## Build Options

### Parallel Builds
```bash
# Build with more cores
nix build .#package --cores 8

# Limit parallel jobs
nix build .#package --max-jobs 4
```

### Offline Mode
```bash
# Build without network access
nix build .#package --offline

# Use only local cache
nix build .#package --no-net
```

### Rebuild Everything
```bash
# Force rebuild (ignore cache)
nix build .#package --rebuild

# Build from source (no binary cache)
nix build .#package --no-substitute
```

## Build Outputs

### Multiple Outputs
```bash
# Packages can have multiple outputs
# Common: out, dev, doc, lib

# Build specific output
nix build .#package.dev    # Development files
nix build .#package.doc    # Documentation
nix build .#package.lib    # Libraries only

# View all outputs
nix show-derivation .#package | jq '.[] | .outputs'
```

### Inspect Build Result
```bash
# After building, result symlink created
ls -l result/

# View store path
readlink result

# Inspect contents
tree result/

# Check dependencies
nix-store -q --references result
nix-store -q --requisites result
```

## Build Caching

### Using Binary Caches
```bash
# Add binary cache
nix build .#package \
  --extra-substituters https://cache.nixos.org \
  --extra-trusted-public-keys cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=

# Use cachix
cachix use mycache
nix build .#package
```

### Push to Cache
```bash
# Push to cachix
nix build .#package
cachix push mycache result

# Push with nix copy
nix copy --to https://cache.example.com result
```

## Build for Development

### Enter Build Environment
```bash
# Enter environment for building
nix develop .#package

# Build manually
unpackPhase
cd $sourceRoot
configurePhase
buildPhase
```

### Iterative Building
```bash
# Build and keep build directory
nix build .#package --keep-failed

# Failed build directory preserved at:
# /tmp/nix-build-package-*.drv-0/

# Inspect failed build
cd /tmp/nix-build-*
```

## Repository-Specific Builds

### Home Repository
```bash
# Build current system
make switch

# Build without switching
make dry-build

# Build specific host
make host/hostname/build

# Build all packages
nix build .#packages.x86_64-linux --all
```

### Build Custom Package
```bash
# Package in pkgs/mypackage/
nix build .#mypackage

# Test package
nix build .#mypackage -L

# Install locally
nix profile install .#mypackage
```

## Debugging Builds

### Verbose Output
```bash
# Show all build output
nix build .#package -L

# Show evaluation trace
nix build .#package --show-trace

# Maximum verbosity
nix build .#package -vvv --show-trace
```

### Build Failures
```bash
# Keep failed build directory
nix build .#package --keep-failed

# Keep successful build directory too
nix build .#package --keep-going

# Inspect build directory
cd /tmp/nix-build-*
ls -la
cat build.log
```

### Interactive Debugging
```bash
# Enter build environment
nix develop .#package

# Run build phases manually
genericBuild
# Or individual phases:
unpackPhase
patchPhase
configurePhase
buildPhase
```

## Build Performance

### Speed Up Builds
```bash
# Use more cores
nix build .#package --cores $(nproc)

# Use binary caches
nix build .#package --substituters https://cache.nixos.org

# Parallel jobs
nix build .#package --max-jobs auto
```

### Monitor Build Progress
```bash
# Real-time build logs
nix build .#package -L

# Follow log file
tail -f /tmp/nix-build-*.log

# Show build statistics
nix build .#package --print-build-logs | ts
```

## Cross-Compilation

### Build for ARM
```bash
# ARM 64-bit
nix build .#package --system aarch64-linux

# ARM 32-bit
nix build .#package --system armv7l-linux

# Using pkgsCross
nix build .#package.pkgsCross.aarch64-multiplatform
```

### Build for Different Platform
```bash
# Build for Raspberry Pi
nix build .#nixosConfigurations.rpi4-host.config.system.build.sdImage

# Build for different NixOS version
nix build github:nixos/nixpkgs/nixos-23.11#package
```

## Advanced Build Techniques

### Override Package Attributes
```bash
# Override during build
nix build --impure --expr '
  (import <nixpkgs> {}).mypackage.overrideAttrs (old: {
    version = "custom";
  })
'
```

### Build from Git
```bash
# Build from specific commit
nix build github:owner/repo/commit-hash#package

# Build from branch
nix build github:owner/repo/branch-name#package

# Build from local git
nix build git+file:///path/to/repo#package
```

### Conditional Builds
```bash
# Build only if changed
nix build .#package --rebuild

# Build with different features
nix build .#package --arg enableFeature true
```

## Build Artifacts

### Store Paths
```bash
# Show store path without building
nix eval .#package

# Show closure size
nix path-info -S .#package

# Show all dependencies
nix-store -qR result

# Show why package is kept
nix-store --query --roots result
```

### Build Logs
```bash
# View previous build log
nix log .#package

# View log of store path
nix log /nix/store/...-package

# Save build log
nix build .#package -L 2>&1 | tee build.log
```

## Build Best Practices

1. **Use binary caches**: Avoid rebuilding common packages
2. **Pin versions**: Use flake.lock for reproducibility
3. **Test locally first**: Build before deploying
4. **Keep build logs**: Debug failures with `-L` flag
5. **Use `--dry-build`**: Preview changes before building
6. **Check closures**: Understand dependencies with `nix path-info`
7. **Incremental builds**: Use `nix develop` for iteration
8. **Parallel builds**: Utilize `--cores` and `--max-jobs`

## Common Build Patterns

### Build and Test
```bash
# Build package
nix build .#mypackage -L

# Run tests
nix build .#mypackage.tests

# Build and run
nix run .#mypackage
```

### Build Multiple Outputs
```bash
# Build all outputs
nix build .#package.all

# Build specific combinations
nix build .#package.out .#package.dev
```

### Build with Local Changes
```bash
# Build with local modifications
nix build .#package --override-input nixpkgs path:/local/nixpkgs

# Build with local flake
nix build path:/local/repo#package
```

## Troubleshooting Builds

### Hash Mismatches
```bash
# Update hash
# 1. Set hash to lib.fakeSha256 or empty string
# 2. Build to get error with correct hash
nix build .#package 2>&1 | grep "got:"

# For fetchFromGitHub
nix-prefetch-github owner repo --rev commit-hash
```

### Disk Space Issues
```bash
# Clean old build artifacts
nix-collect-garbage -d

# Check store size
du -sh /nix/store

# Remove specific paths
nix-store --delete /nix/store/...-package
```

### Permission Errors
```bash
# Build as root
sudo nix build .#package

# Build with different user
sudo -u builder nix build .#package
```

## Resources

- [Nix Build Manual](https://nixos.org/manual/nix/stable/command-ref/nix-build.html)
- [NixOS Rebuild](https://nixos.org/manual/nixos/stable/#sec-changing-config)
- [Nix Package Stdenv](https://nixos.org/manual/nixpkgs/stable/#chap-stdenv)
