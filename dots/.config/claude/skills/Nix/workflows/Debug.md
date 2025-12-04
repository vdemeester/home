# Debug Workflow

Debug Nix expressions, evaluation errors, and build failures.

## When to Use

- "debug nix expression"
- "nix evaluation error"
- "troubleshoot nix build"
- "nix error messages"

## Quick Commands

### Evaluation Debugging
```bash
# Show evaluation trace
nix eval .#package --show-trace

# Evaluate with verbosity
nix eval .#package -vvv

# Show derivation
nix show-derivation .#package

# Inspect attribute path
nix eval .#nixosConfigurations.hostname.config.services
```

### Build Debugging
```bash
# Build with full logs
nix build .#package -L
nix build .#package --print-build-logs

# Keep failed build directory
nix build .#package --keep-failed

# Show trace on error
nix build .#package --show-trace

# Maximum verbosity
nix build .#package -vvv --show-trace -L
```

### Interactive Debugging
```bash
# Enter build environment
nix develop .#package

# Enter failed build directory
cd /tmp/nix-build-*

# Run phases manually
unpackPhase
patchPhase
configurePhase
buildPhase
```

## Understanding Error Messages

### Infinite Recursion
```
error: infinite recursion encountered
```

**Cause**: Circular dependency or self-referencing attribute

**Debug**:
```nix
# Problem
{
  x = y;
  y = x;  # Infinite recursion!
}

# Fix: Break the cycle
{
  x = lib.mkDefault value;
  y = config.x;
}
```

### Attribute Not Found
```
error: attribute 'foo' missing
```

**Debug**:
```bash
# Check what attributes exist
nix eval .#nixosConfigurations.hostname --apply builtins.attrNames

# Check nested attributes
nix eval .#nixosConfigurations.hostname.config --apply "x: builtins.attrNames x.services"

# Use --show-trace to see where error originates
nix eval .#package --show-trace
```

### Type Errors
```
error: value is a set while a string was expected
```

**Debug**:
```bash
# Check type of value
nix eval .#value --apply builtins.typeOf

# Print value for inspection
nix eval .#value --apply "x: builtins.trace x x"

# Show detailed type error
nix eval .#value --show-trace
```

### Hash Mismatches
```
error: hash mismatch in fixed-output derivation
  got:    sha256-AAAA...
  wanted: sha256-BBBB...
```

**Fix**:
```bash
# Copy the "got" hash to your expression
# Or use nix-prefetch for correct hash
nix-prefetch-url https://example.com/file.tar.gz
nix-prefetch-github owner repo --rev commit-hash
```

## Debugging Techniques

### Trace Values
```nix
# Simple trace
value = builtins.trace "Debug: ${toString someValue}" someValue;

# Trace with lib
value = lib.traceVal someValue;
value = lib.traceValSeq "message" someValue;

# Trace attribute names
value = lib.traceSeq (builtins.attrNames attrs) attrs;

# Deep trace
value = lib.traceValSeqN 3 someNestedValue;
```

### Evaluate Expressions
```bash
# Evaluate simple expression
nix eval --expr '1 + 1'

# Evaluate with nixpkgs
nix eval --expr 'with import <nixpkgs> {}; lib.version'

# Evaluate flake attribute
nix eval .#nixosConfigurations.hostname.config.networking.hostName

# Evaluate and show as JSON
nix eval .#package --json

# Evaluate and show as XML
nix eval .#package --xml
```

### Check Configuration
```bash
# Check NixOS configuration validity
nixos-rebuild build --flake .#hostname --dry-run

# Show configuration options
nix eval .#nixosConfigurations.hostname.options

# Check if option exists
nix eval .#nixosConfigurations.hostname.options.services.nginx.enable.defined

# Show option documentation
nixos-option services.nginx.enable
```

## Debugging Build Failures

### Inspect Build Environment
```bash
# Enter build environment
nix develop .#package

# Check environment variables
env | grep -i nix
printenv

# Check build tools available
which gcc make cmake

# Check paths
echo $PATH
echo $PKG_CONFIG_PATH
```

### Manual Build Steps
```bash
# Enter development shell
nix develop .#package

# Run phases manually
unpackPhase
ls -la

cd $sourceRoot
patchPhase

configurePhase
# Fix any configuration issues

buildPhase
# Fix build errors

checkPhase
installPhase
```

### Inspect Failed Build
```bash
# Build with --keep-failed
nix build .#package --keep-failed

# Failed build kept at:
cd /tmp/nix-build-package-*.drv-0/

# Examine source
ls -la
find . -type f -name "*.log"

# Check build log
cat build.log

# Re-run failed command manually
./configure
make
```

### Debug Build Script
```bash
# Show builder script
nix show-derivation .#package | jq '.[].env.builder'

# Show full derivation
nix derivation show .#package

# Print build environment
nix develop .#package --command printenv
```

## Debugging Evaluation Errors

### Stack Traces
```bash
# Show full stack trace
nix eval .#package --show-trace

# Example output:
# error: undefined variable 'foo'
#
#        at /path/to/file.nix:42:5:
#           41|   bar = {
#           42|     baz = foo;
#              |           ^
#           43|   };
```

### Find Definition Location
```bash
# Find where option is defined
nix eval .#nixosConfigurations.hostname.options.services.nginx.enable.definitionsWithLocations

# Find where value is set
nix eval .#nixosConfigurations.hostname.options.services.nginx.enable.files
```

### Check Module Evaluation
```bash
# Evaluate specific module
nix eval -f ./module.nix

# Check module with config
nix eval --expr 'import ./module.nix { lib = import <nixpkgs/lib>; }'

# Show module errors
nix-instantiate --eval --strict ./module.nix
```

## Debugging Package Builds

### Check Dependencies
```bash
# Show build dependencies
nix-store -q --references $(nix-build -A package)

# Show runtime dependencies
nix-store -q --requisites $(nix-build -A package)

# Show dependency tree
nix-store -q --tree $(nix-build -A package)

# Why is package kept?
nix-store --query --roots $(nix-build -A package)
```

### Missing Dependencies
```bash
# Check what's needed
ldd result/bin/program

# Patch with nix
nix develop -c ldd ./program

# Auto-patch with autoPatchelfHook
nativeBuildInputs = [ autoPatchelfHook ];
```

### Wrong Version Built
```bash
# Check what will be built
nix-build '<nixpkgs>' -A package --dry-run

# Check version in store
nix-store -q --hash $(nix-build -A package)

# Force specific version
nix build nixpkgs/nixos-23.11#package
```

## Debugging NixOS Configurations

### Configuration Evaluation
```bash
# Build configuration
nixos-rebuild build --flake .#hostname --show-trace

# Dry build to see what changes
nixos-rebuild dry-build --flake .#hostname

# Evaluate specific option
nix eval .#nixosConfigurations.hostname.config.system.stateVersion
```

### Module Conflicts
```
error: The option `services.foo.enable' is defined multiple times
```

**Debug**:
```bash
# Find all definitions
nix eval .#nixosConfigurations.hostname.options.services.foo.enable.definitionsWithLocations

# Check module imports
nix eval .#nixosConfigurations.hostname.options._module.args --apply "x: builtins.attrNames x"
```

### Service Failures
```bash
# Check service status after deployment
systemctl status servicename

# Check service logs
journalctl -u servicename -n 50

# Check generated config
systemctl cat servicename

# Verify config file
cat /etc/systemd/system/servicename.service
```

## Debugging Flakes

### Flake Evaluation
```bash
# Check flake
nix flake check

# Show flake outputs
nix flake show

# Evaluate flake metadata
nix flake metadata

# Lock file issues
nix flake lock --update-input nixpkgs
```

### Input Problems
```bash
# Check inputs
nix flake metadata | grep -A 10 Inputs

# Update specific input
nix flake update nixpkgs

# Show input derivation
nix flake metadata --json | jq .locks
```

### Pure Evaluation Errors
```
error: access to absolute path '/home/...' is forbidden in pure eval mode
```

**Fix**:
```nix
# Don't use absolute paths
# Bad
path = /home/user/file.nix;

# Good
path = ./file.nix;

# Or use --impure flag (not recommended)
nix build --impure
```

## Advanced Debugging

### Debug with nix repl
```bash
# Start repl
nix repl

# Load flake
:lf .

# Explore outputs
outputs.packages.x86_64-linux

# Load nixpkgs
:l <nixpkgs>

# Test expressions
lib.version
pkgs.hello

# Tab completion works
outputs.nixosConf[TAB]
```

### Profile Evaluation
```bash
# Time evaluation
time nix eval .#package

# Show evaluation statistics
nix eval .#package --show-stats

# Profile build
nix build .#package --profile /tmp/profile
```

### Debug with breakpoints
```nix
# In your expression
value = builtins.break (someExpression);

# Build will pause, entering debugger
# Commands:
# :continue - Continue evaluation
# :quit - Abort evaluation
# :env - Show environment
# :value - Show current value
```

## Common Debug Patterns

### Print and Return
```nix
# Debug helper
debug = msg: val: builtins.trace msg val;

# Usage
result = debug "Processing ${name}" (processValue value);
```

### Conditional Debugging
```nix
# Only debug when flag is set
let
  debugFlag = false;
  debug = msg: val:
    if debugFlag
    then builtins.trace msg val
    else val;
in
  debug "Value" someValue
```

### Assert Debugging
```nix
# Use assertions to catch errors early
{ ... }:

assert someCondition; {
  # Your config
}

# Better with message
{ lib, ... }:

lib.assertMsg someCondition "Error: condition not met"
```

## Debugging Tools

### nix-tree
```bash
# Install
nix-shell -p nix-tree

# Explore dependencies interactively
nix-tree $(nix-build -A package)
```

### nix-diff
```bash
# Compare two derivations
nix-diff $(nix-build -A package1) $(nix-build -A package2)
```

### nix-du
```bash
# Analyze disk usage
nix-du -s=500MB

# Show largest paths
nix-du | head -20
```

## Debugging Best Practices

1. **Use --show-trace**: Always show full stack traces
2. **Keep failed builds**: Use `--keep-failed` to inspect
3. **Enable verbose logs**: Use `-L` flag for build logs
4. **Use nix repl**: Interactive exploration is powerful
5. **Check documentation**: Many options have examples
6. **Test in isolation**: Build packages separately
7. **Use nix develop**: Debug in build environment
8. **Check flake.lock**: Ensure inputs are correct

## Emergency Debugging

### System Won't Boot
```bash
# Boot to previous generation (at boot menu)
# Or from rescue system:

# Mount nixos
mount /dev/sdX /mnt
mount /dev/sdY /mnt/boot

# Chroot
nixos-enter

# List generations
nix-env --list-generations --profile /nix/var/nix/profiles/system

# Rollback
/nix/var/nix/profiles/system-42-link/bin/switch-to-configuration switch
```

### Out of Disk Space
```bash
# Clean old generations
nix-collect-garbage -d

# Delete specific generation
nix-env --delete-generations 10 11 12

# Optimize store
nix-store --optimize

# Find large paths
du -sh /nix/store/* | sort -h | tail -20
```

## Resources

- [Nix Manual - Debugging](https://nixos.org/manual/nix/stable/language/operators.html#debugging)
- [NixOS Debugging Guide](https://nixos.wiki/wiki/Debugging)
- [Nix Pills - Debugging](https://nixos.org/guides/nix-pills/debugging.html)
