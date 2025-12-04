# Troubleshoot Workflow

Common Nix issues, error messages, and solutions.

## When to Use

- "nix error"
- "troubleshoot nix"
- "nix build failing"
- "fix nix issue"

## Common Errors

### Hash Mismatch

**Error:**
```
error: hash mismatch in fixed-output derivation
  specified: sha256-AAAA...
  got:        sha256-BBBB...
```

**Solution:**
```nix
# Copy the "got" hash
src = fetchFromGitHub {
  owner = "owner";
  repo = "repo";
  rev = "v1.0.0";
  hash = "sha256-BBBB...";  # Use hash from error
};
```

**Get Hash Correctly:**
```bash
# For GitHub
nix-prefetch-github owner repo --rev v1.0.0

# For URLs
nix-prefetch-url https://example.com/file.tar.gz

# For Git
nix-prefetch-git https://git.example.com/repo.git --rev commit-hash
```

### Infinite Recursion

**Error:**
```
error: infinite recursion encountered
```

**Common Causes:**
```nix
# 1. Self-referencing attribute
{
  x = x;  # Infinite recursion!
}

# 2. Circular dependency
{
  a = b;
  b = a;  # Infinite recursion!
}

# 3. Overlay recursion
final: prev: {
  mypackage = prev.mypackage.override {  # Infinite!
    # ...
  };
}
```

**Solutions:**
```nix
# Use lib.mkDefault or lib.mkForce
{ lib, ... }:
{
  value = lib.mkDefault something;
}

# Break overlay recursion with super
final: prev: {
  mypackage = (prev.mypackage.override {
    # ...
  }).overrideAttrs (old: {
    # ...
  });
}

# Use rec carefully
rec {
  x = 1;
  y = x + 1;  # OK
}
```

### Attribute Not Found

**Error:**
```
error: attribute 'foo' missing
error: undefined variable 'bar'
```

**Debug:**
```bash
# Check what attributes exist
nix eval .#nixosConfigurations.hostname --apply builtins.attrNames

# Check nested path
nix eval .#nixosConfigurations.hostname.config.services --apply builtins.attrNames

# Use --show-trace
nix eval .#value --show-trace
```

**Common Fixes:**
```nix
# Check spelling
services.nginx.enable = true;  # Not nginX or nginx

# Import missing module
imports = [ ./missing-module.nix ];

# Check if package exists
nix search nixpkgs package-name
```

### File Not Found

**Error:**
```
error: getting status of '/nix/store/.../file': No such file or directory
error: path '/path/to/file' does not exist
```

**Solutions:**
```nix
# Use relative paths in flakes
src = ./path/to/file;  # Not /absolute/path

# Check file exists
# ls path/to/file

# In pure eval, paths must be relative
# Don't use: /home/user/file
# Use: ./file or ./relative/path
```

### Import from Derivation (IFD)

**Error:**
```
error: cannot build '/nix/store/...' during evaluation
```

**Cause:**
Import from Derivation - importing result of a build

**Workarounds:**
```nix
# Avoid when possible
# Bad:
let
  generated = import (pkgs.runCommand "gen" {} ''
    echo "{ value = 42; }" > $out
  '');
in
  generated.value

# Good: Use builtins or pure Nix
let
  generated = { value = 42; };
in
  generated.value
```

### Restricted Mode / Pure Eval

**Error:**
```
error: access to absolute path '/home/...' is forbidden in pure eval mode
error: cannot look up '<nixpkgs>' in pure evaluation mode
```

**Solutions:**
```nix
# Use relative paths
path = ./file.nix;  # Not /absolute/path

# Use flake inputs instead of <nixpkgs>
{ inputs, ... }:
  inputs.nixpkgs.legacyPackages.x86_64-linux.hello

# For development, use --impure
nix build --impure
```

## Build Failures

### Compilation Errors

**Check Build Log:**
```bash
# View last build log
nix log .#package

# Keep build directory on failure
nix build .#package --keep-failed

# Inspect failed build
cd /tmp/nix-build-package-*.drv-0/
cat build.log
ls -la
```

**Common Issues:**
```bash
# Missing dependencies
# Add to buildInputs or nativeBuildInputs

# Wrong C/C++ compiler
# Set CC, CXX environment variables

# Missing pkg-config
# Add pkg-config to nativeBuildInputs

# Library not found
# Add to buildInputs and check PKG_CONFIG_PATH
```

### Test Failures

**Skip Tests Temporarily:**
```nix
stdenv.mkDerivation {
  # ...
  doCheck = false;  # Disable tests
}
```

**Debug Tests:**
```bash
# Run tests manually
nix develop .#package
unpackPhase
cd $sourceRoot
configurePhase
buildPhase
checkPhase  # Run tests with full output
```

### Missing Files in Output

**Check Output:**
```bash
# What's in the output?
nix build .#package
tree result/
ls -R result/

# Check if files were created during build
nix build .#package --keep-failed
cd /tmp/nix-build-*/
find . -name "expected-file"
```

**Fix Install Phase:**
```nix
installPhase = ''
  runHook preInstall

  mkdir -p $out/bin
  cp myapp $out/bin/

  # Install other files
  mkdir -p $out/share/doc
  cp README.md $out/share/doc/

  runHook postInstall
'';
```

## Runtime Issues

### Executable Not Found

**Error:**
```
bash: command not found
```

**Check:**
```bash
# Is package installed?
which command-name

# Check package output
nix build .#package
ls result/bin/

# Check PATH
echo $PATH

# Install package
nix profile install .#package
```

### Library Not Found

**Error:**
```
error while loading shared libraries: libfoo.so.1: cannot open shared object file
```

**Solutions:**
```bash
# Use autoPatchelfHook
nativeBuildInputs = [ autoPatchelfHook ];

# Add runtime dependencies
buildInputs = [ libfoo ];

# Check dependencies
ldd result/bin/program

# Patch manually
patchelf --set-rpath ${lib.makeLibraryPath [ libfoo ]} $out/bin/program
```

### Permission Denied

**Check Permissions:**
```bash
# File permissions in store
ls -l result/

# Files should be readable
# Executables should have +x
```

**Fix:**
```nix
postInstall = ''
  chmod +x $out/bin/myapp
'';
```

## Disk Space Issues

### Out of Disk Space

**Check Space:**
```bash
# Check /nix/store size
du -sh /nix/store

# Check total disk usage
df -h

# Find large paths
nix path-info --closure-size .#package | sort -h
```

**Clean Up:**
```bash
# Delete old generations
nix-collect-garbage -d

# Delete generations older than 30 days
nix-collect-garbage --delete-older-than 30d

# Optimize store (deduplicate)
nix-store --optimize

# Delete specific paths
nix-store --delete /nix/store/...-package
```

### Garbage Collection Issues

**Error:**
```
error: cannot delete path '...' because it is in use by '...'
```

**Solutions:**
```bash
# Find what's using the path
nix-store --query --roots /nix/store/...-package

# Remove from profiles
nix profile remove package-name

# Delete old generations first
nix-env --delete-generations old
nix-collect-garbage
```

## Network Issues

### Download Failures

**Error:**
```
error: unable to download 'https://...'
error: curl error: Connection timeout
```

**Solutions:**
```bash
# Check network
ping example.com

# Try with different substituter
nix build --substituters https://cache.nixos.org

# Build without network (use only local)
nix build --offline

# Build without binary cache
nix build --no-substitute
```

### Binary Cache Issues

**Error:**
```
error: cannot add path '...' to the store
warning: ignoring substitute for '...'
```

**Solutions:**
```bash
# Add cache manually
nix build --substituters https://cache.nixos.org \
  --trusted-public-keys cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=

# Configure in nix.conf
# trusted-substituters = https://cache.nixos.org

# Use cachix
cachix use cachename
```

## Configuration Issues

### Syntax Errors

**Error:**
```
error: syntax error, unexpected IF
error: undefined variable 'if'
```

**Common Mistakes:**
```nix
# 1. Missing semicolon
{
  option1 = value1
  option2 = value2;  # ERROR: missing ; after value1
}

# 2. Wrong if syntax
if condition value1 value2  # ERROR: missing 'then' and 'else'
if condition then value1 else value2  # Correct

# 3. Missing 'in'
let x = 1; x + 1  # ERROR: missing 'in'
let x = 1; in x + 1  # Correct

# 4. Comments
/* Multi-line comment */
# Single-line comment
```

### Module Conflicts

**Error:**
```
error: The option `services.foo.enable' is defined multiple times
```

**Solutions:**
```nix
# Use lib.mkForce to override
services.foo.enable = lib.mkForce true;

# Or remove duplicate definition
# Check all imported modules

# Use lib.mkMerge for lists
services.foo.extraConfig = lib.mkMerge [
  (lib.mkIf condition1 [ value1 ])
  (lib.mkIf condition2 [ value2 ])
];
```

### Type Mismatches

**Error:**
```
error: value is a set while a string was expected
error: value is a string while a list was expected
```

**Solutions:**
```nix
# Check types
builtins.typeOf value

# Convert as needed
toString 42  # "42"
lib.splitString "," "a,b,c"  # [ "a" "b" "c" ]
[ value ]  # Wrap in list

# Check option type
# Look at option definition to see expected type
```

## System Issues

### System Won't Boot

**Recovery:**
```bash
# 1. Boot to previous generation
# Select older generation in bootloader menu

# 2. From rescue system
mount /dev/sdX /mnt
mount /dev/sdY /mnt/boot
nixos-enter --root /mnt

# Rollback
nixos-rebuild switch --rollback

# Or specific generation
/nix/var/nix/profiles/system-42-link/bin/switch-to-configuration switch
```

### Service Failures

**Debug:**
```bash
# Check service status
systemctl status service-name

# View logs
journalctl -u service-name -n 50

# View service definition
systemctl cat service-name

# Check generated config
cat /etc/systemd/system/service-name.service
```

**Common Issues:**
```nix
# Wrong user
systemd.services.myservice.serviceConfig.User = "correct-user";

# Missing dependency
systemd.services.myservice.after = [ "network.target" ];

# Wrong paths
# Use full store paths or ${}
ExecStart = "${pkgs.myapp}/bin/myapp";
```

### Home-Manager Issues

**Error:**
```
error: collision between files
```

**Solutions:**
```nix
# Let home-manager manage conflicts
home.file."conflict".source = ./file;

# Force overwrite
home.file."conflict" = {
  source = ./file;
  force = true;
};

# Disable conflicting package
home.packages = lib.filter (p: p != pkgs.conflicting) config.home.packages;
```

## Performance Issues

### Slow Evaluation

**Profile:**
```bash
# Time evaluation
time nix eval .#nixosConfigurations.hostname.config.system.build.toplevel

# Show stats
nix eval --show-stats .#value
```

**Optimize:**
```nix
# Avoid expensive operations in hot paths
# Use builtins when possible
# Minimize use of import
# Cache computed values
```

### Slow Builds

**Speed Up:**
```bash
# Use more cores
nix build --cores $(nproc)

# Parallel jobs
nix build --max-jobs auto

# Use binary caches
nix build --substituters https://cache.nixos.org

# Build without downloads
nix build --offline
```

## Emergency Recovery

### Corrupted Nix Store

**Check:**
```bash
# Verify store
nix-store --verify --check-contents

# Repair
nix-store --verify --repair
```

### Locked Nix Database

**Error:**
```
error: cannot open Nix database: database is locked
```

**Fix:**
```bash
# Stop nix-daemon
systemctl stop nix-daemon

# Remove lock
rm /nix/var/nix/db/db.lock

# Restart daemon
systemctl start nix-daemon
```

### Full Disk

**Emergency Clean:**
```bash
# Delete old generations
sudo nix-env --delete-generations +5

# Garbage collect
sudo nix-collect-garbage -d

# Optimize
sudo nix-store --optimize

# Remove build artifacts
rm -rf /tmp/nix-build-*
```

## Resources

- [Nix Error Messages](https://nixos.org/manual/nix/stable/language/index.html)
- [NixOS Troubleshooting](https://nixos.wiki/wiki/Troubleshooting)
- [Nix Pills - Debugging](https://nixos.org/guides/nix-pills/debugging.html)
- [Common Issues](https://nixos.wiki/wiki/FAQ)
