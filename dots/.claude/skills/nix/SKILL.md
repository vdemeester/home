# Nix Best Practices

## Purpose
Expert guidance on Nix, NixOS, and home-manager following best practices.

## Core Principles

### 1. Declarative Configuration Over Imperative
```nix
# Good: Declarative
services.nginx.enable = true;

# Bad: Imperative
systemd.services.nginx.postStart = "systemctl start nginx";
```

### 2. Reproducibility
Same inputs = Same outputs
- Pin versions explicitly
- Use flake.lock for consistency
- Avoid impure operations

### 3. Modularity
Break configurations into focused, reusable modules
```nix
# Good: Modular
imports = [
  ./hardware.nix
  ./networking.nix
  ./services.nix
];

# Bad: Everything in one file
```

### 4. Version Control Everything
- Track all Nix configurations in git
- Commit flake.lock changes
- Document why changes were made

### 5. Use Flakes for Modern Nix
Flakes provide:
- Hermetic evaluation
- Standardized structure
- Dependency locking
- Better caching

## NixOS Configuration Patterns

### Host Configuration Structure
```nix
# systems/<hostname>/
├── boot.nix       # Bootloader, initrd, kernel modules
├── hardware.nix   # Hardware settings, filesystems, mounts
├── extra.nix      # Optional: additional host-specific config
└── home.nix       # Optional: host-specific home-manager config
```

### Using mkHost Pattern
```nix
# In flake.nix
nixosConfigurations = {
  hostname = libx.mkHost {
    hostname = "hostname";
    system = "x86_64-linux";
    hardwareType = "desktop";  # or "rpi4"
    desktop = "sway";          # or "niri", or null
    nixpkgs = nixpkgs;         # or nixpkgs-25_05 for stable
  };
};
```

### Common Module Organization
```
systems/common/
├── base/          # Essential base configuration
├── desktop/       # Desktop environment configs
├── hardware/      # Hardware-specific modules
├── programs/      # Application configurations
├── services/      # System services
└── users/         # User account definitions
```

### Checking globals.nix
Always check `globals.nix` for:
- Machine definitions (IPs, SSH keys)
- DNS zone configurations
- VPN settings
- Syncthing device IDs
- Network topology

## Module Best Practices

### Define Options Properly
```nix
{ config, lib, pkgs, ... }:

{
  options = {
    services.myservice = {
      enable = lib.mkEnableOption "my service";

      port = lib.mkOption {
        type = lib.types.port;
        default = 8080;
        description = "Port to listen on";
      };

      configFile = lib.mkOption {
        type = lib.types.path;
        description = "Path to configuration file";
      };
    };
  };

  config = lib.mkIf config.services.myservice.enable {
    # Implementation
  };
}
```

### Use Types Correctly
Common types:
- `types.bool` - Boolean values
- `types.int` - Integers
- `types.str` - Strings
- `types.path` - File system paths
- `types.port` - Network ports (1-65535)
- `types.listOf types.str` - Lists
- `types.attrs` - Attribute sets
- `types.package` - Nix packages

### Leverage mkIf, mkMerge, mkDefault
```nix
# Conditional configuration
config = lib.mkIf config.services.myservice.enable {
  # ...
};

# Merge multiple configurations
config = lib.mkMerge [
  { always.present = true; }
  (lib.mkIf condition { conditional.value = true; })
];

# Provide defaults that can be overridden
services.myservice.port = lib.mkDefault 8080;
```

## Package Development

### Use callPackage Pattern
```nix
# In pkgs/default.nix
{
  mypackage = pkgs.callPackage ./mypackage { };
  mytool = pkgs.callPackage ./mytool { };
}
```

### Package Definition
```nix
# pkgs/mypackage/default.nix
{ lib
, stdenv
, fetchFromGitHub
, buildGoModule  # or rustPlatform, python3Packages, etc.
}:

buildGoModule rec {
  pname = "mypackage";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "owner";
    repo = "repo";
    rev = "v${version}";
    hash = "sha256-...";
  };

  vendorHash = "sha256-...";

  meta = with lib; {
    description = "Package description";
    homepage = "https://example.com";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    platforms = platforms.linux;
  };
}
```

### Using Overlays
```nix
# overlays/default.nix
{ inputs }:
{
  additions = final: _prev: import ../pkgs { pkgs = final; };

  modifications = final: prev: {
    # Override existing packages
    somepackage = prev.somepackage.overrideAttrs (old: {
      version = "custom";
    });
  };
}
```

## Flake Management

### Essential Commands
```bash
# Lock dependencies
nix flake lock

# Update all inputs
nix flake update

# Update specific input
nix flake update nixpkgs

# Check flake validity
nix flake check

# Show flake outputs
nix flake show

# Show flake metadata
nix flake metadata
```

### Flake Structure
```nix
{
  description = "Flake description";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
    nixosConfigurations = { ... };
    homeConfigurations = { ... };
    packages = { ... };
    devShells = { ... };
  };
}
```

## Home-Manager Patterns

### Environment Variables
```nix
home.sessionVariables = {
  EDITOR = "vim";
  VISUAL = "vim";
  BROWSER = "firefox";
};
```

### XDG Config Files
```nix
# Symlink static files
xdg.configFile."myapp/config.yml".source = ./myapp-config.yml;

# Generate files dynamically
xdg.configFile."myapp/generated.conf".text = ''
  setting1 = ${someValue}
  setting2 = value2
'';

# Make executable
xdg.configFile."bin/script.sh" = {
  source = ./script.sh;
  executable = true;
};
```

### Services
```nix
# User-level systemd service
systemd.user.services.myservice = {
  Unit = {
    Description = "My Service";
    After = [ "network.target" ];
  };

  Service = {
    ExecStart = "${pkgs.mypackage}/bin/myservice";
    Restart = "on-failure";
  };

  Install = {
    WantedBy = [ "default.target" ];
  };
};
```

## Secrets Management with agenix

### Define Secrets
```nix
# secrets.nix
let
  user = "ssh-ed25519 AAAAC3...";
  system = "ssh-ed25519 AAAAC3...";
in {
  "secret.age".publicKeys = [ user system ];
}
```

### Use Secrets in Configuration
```nix
{
  age.secrets.mySecret = {
    file = ../secrets/mySecret.age;
    owner = "myuser";
    group = "mygroup";
  };

  # Reference in config
  services.myservice.passwordFile = config.age.secrets.mySecret.path;
}
```

### Encrypt Secrets
```bash
# Encrypt a secret
agenix -e secrets/mySecret.age

# Re-key all secrets
agenix -r
```

## Safety and Testing

### Build Without Switching
```bash
# Build configuration
nixos-rebuild build --flake .#<hostname>

# Dry run (show what would change)
nixos-rebuild dry-build --flake .#<hostname>

# Test without adding to bootloader
nixos-rebuild test --flake .#<hostname>
```

### Rollback Strategy
```bash
# List generations
nixos-rebuild list-generations

# Rollback to previous generation
nixos-rebuild switch --rollback

# Switch to specific generation
nixos-rebuild switch --switch-generation <number>
```

### Keep Old Generations
- Never delete all old generations
- Keep at least 2-3 recent generations for rollback
- Clean periodically with: `nix-collect-garbage -d`

## Common Patterns

### Conditional Imports
```nix
imports = [
  ./base.nix
] ++ lib.optionals (desktop != null) [
  ./desktop/${desktop}
];
```

### String Interpolation
```nix
# Simple
message = "Hello ${name}";

# Multi-line
config = ''
  setting1 = ${value1}
  setting2 = ${value2}
'';

# Escape $
script = ''
  echo "Nix variable: ${nixVar}"
  echo "Shell variable: ''${shellVar}"
'';
```

### List Operations
```nix
# Concatenation
all = list1 ++ list2;

# Filter
filtered = lib.filter (x: x > 5) list;

# Map
doubled = map (x: x * 2) list;
```

### Attribute Set Operations
```nix
# Merge
merged = set1 // set2;

# Recursive merge
merged = lib.recursiveUpdate set1 set2;

# Filter attributes
filtered = lib.filterAttrs (n: v: v != null) attrs;

# Map attributes
mapped = lib.mapAttrs (n: v: v * 2) attrs;
```

## Debugging

### Print Values
```nix
# Use lib.traceVal for debugging
value = lib.traceVal someExpression;

# Trace with message
value = lib.traceValSeq "message" someExpression;
```

### Evaluate Expressions
```bash
# Evaluate Nix expression
nix eval .#nixosConfigurations.hostname.config.services.nginx.enable

# Show derivation
nix show-derivation .#package

# Inspect store path
nix path-info .#package
```

### Common Issues

#### Hash Mismatch
```bash
# Update hash for fetchFromGitHub
nix-prefetch-github owner repo --rev <commit-hash>

# Update vendor hash for Go modules
# Set vendorHash = lib.fakeSha256;
# Build will fail with correct hash
```

#### Import Cycles
- Check for circular imports
- Use `lib.mkIf` to break cycles
- Restructure module organization

## Performance

### Build Optimization
- Use binary caches
- Avoid rebuilding unnecessarily
- Keep flake.lock updated but stable
- Use `nix-direnv` for development shells

### Evaluation Speed
- Minimize use of `import`
- Use `builtins` wisely
- Avoid expensive list operations in hot paths

## Resources

- NixOS Manual: https://nixos.org/manual/nixos/stable/
- Nix Package Manual: https://nixos.org/manual/nixpkgs/stable/
- Home-Manager Manual: https://nix-community.github.io/home-manager/
- Nix Pills: https://nixos.org/guides/nix-pills/

## Repository-Specific Patterns

### For ~/src/home Repository

#### Adding a New Host
1. Create `/systems/<hostname>` with `boot.nix`, `hardware.nix`
2. Add to `flake.nix` using `libx.mkHost`
3. Update `globals.nix` with machine metadata
4. Add to `secrets.nix` if using secrets

#### Adding a New Package
1. Create `/pkgs/<package-name>/default.nix`
2. Add to `/pkgs/default.nix` with `callPackage`
3. Test with `nix build .#<package-name>`

#### Common Commands
- Build: `make switch`
- Format: `make fmt`
- Clean: `make clean`
- Deploy: `make host/<hostname>/switch` (ask first!)

Remember: Nix is about reproducibility and declarative configuration. When in doubt, consult the manuals and follow the patterns established in the repository.
