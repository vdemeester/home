# Flakes Workflow

Work with Nix flakes: create, update, manage inputs and outputs.

## When to Use

- "create nix flake"
- "update flake inputs"
- "flake.lock"
- "nix flake"

## Quick Commands

### Basic Flake Commands
```bash
# Initialize new flake
nix flake init

# Initialize with template
nix flake init -t templates#simple

# Show flake outputs
nix flake show

# Show flake metadata
nix flake metadata

# Check flake validity
nix flake check

# Update all inputs
nix flake update

# Update specific input
nix flake update nixpkgs
```

### Lock File Management
```bash
# Lock dependencies
nix flake lock

# Update lock file
nix flake update

# Update specific input
nix flake lock --update-input nixpkgs

# Show lock file info
nix flake metadata --json | jq .locks
```

## Flake Structure

### Basic Flake Template
```nix
{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux.hello;
  };
}
```

### Complete Flake Template
```nix
{
  description = "Complete flake example";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    # Packages
    packages.${system} = {
      default = pkgs.hello;
      myapp = pkgs.callPackage ./pkgs/myapp { };
    };

    # NixOS configurations
    nixosConfigurations = {
      hostname = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [ ./configuration.nix ];
      };
    };

    # Home-manager configurations
    homeConfigurations = {
      "user@hostname" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./home.nix ];
      };
    };

    # Development shells
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = with pkgs; [ go gopls ];
    };
  };
}
```

## Flake Inputs

### Input Formats
```nix
inputs = {
  # GitHub repository (latest)
  nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  # Specific branch
  nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.05";

  # Specific commit
  nixpkgs-pinned.url = "github:nixos/nixpkgs/abc123...";

  # GitLab
  myrepo.url = "gitlab:user/repo";

  # Git repository
  mylib.url = "git+https://git.example.com/repo.git";

  # Local path
  locallib.url = "path:/home/user/projects/lib";

  # Flake in subdirectory
  subflake.url = "github:owner/repo?dir=subdir";

  # With specific ref
  tagged.url = "github:owner/repo/v1.0.0";
};
```

### Following Inputs
```nix
inputs = {
  nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  # Make home-manager use same nixpkgs
  home-manager = {
    url = "github:nix-community/home-manager";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  # Multiple follows
  package = {
    url = "github:owner/repo";
    inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-utils.follows = "flake-utils";
    };
  };
};
```

## Flake Outputs

### Output Types
```nix
outputs = { self, nixpkgs }: {
  # Packages
  packages.x86_64-linux.myapp = /* derivation */;

  # Default package
  packages.x86_64-linux.default = /* derivation */;

  # Apps
  apps.x86_64-linux.myapp = {
    type = "app";
    program = "${self.packages.x86_64-linux.myapp}/bin/myapp";
  };

  # Development shells
  devShells.x86_64-linux.default = /* mkShell */;

  # NixOS configurations
  nixosConfigurations.hostname = /* nixosSystem */;

  # Home-manager configurations
  homeConfigurations."user@host" = /* homeManagerConfiguration */;

  # Overlays
  overlays.default = final: prev: {
    mypackage = /* derivation */;
  };

  # NixOS modules
  nixosModules.default = /* module */;

  # Checks
  checks.x86_64-linux.test = /* derivation */;

  # Formatter
  formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixfmt;
};
```

### Per-System Outputs
```nix
{
  outputs = { self, nixpkgs }: let
    # Helper to generate for each system
    forAllSystems = nixpkgs.lib.genAttrs [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];
  in {
    packages = forAllSystems (system: {
      default = nixpkgs.legacyPackages.${system}.hello;
    });

    devShells = forAllSystems (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      default = pkgs.mkShell {
        buildInputs = [ pkgs.go ];
      };
    });
  };
}
```

### Using flake-utils
```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      packages.default = pkgs.hello;
      devShells.default = pkgs.mkShell {
        buildInputs = [ pkgs.go ];
      };
    });
}
```

## Managing Lock Files

### Lock File Structure
```json
{
  "nodes": {
    "nixpkgs": {
      "locked": {
        "lastModified": 1234567890,
        "narHash": "sha256-...",
        "owner": "nixos",
        "repo": "nixpkgs",
        "rev": "abc123...",
        "type": "github"
      },
      "original": {
        "owner": "nixos",
        "ref": "nixos-unstable",
        "repo": "nixpkgs",
        "type": "github"
      }
    }
  }
}
```

### Lock File Operations
```bash
# Create or update lock file
nix flake lock

# Update all inputs
nix flake update

# Update single input
nix flake lock --update-input nixpkgs

# Update and commit
nix flake update
git add flake.lock
git commit -m "chore: update flake inputs"
```

### Pin Specific Revision
```nix
inputs = {
  # Pin to specific commit
  nixpkgs.url = "github:nixos/nixpkgs/abc123def456";
  # Or in lock file manually, then:
};
```

```bash
# Lock without updating
nix flake lock --no-update-lock-file

# Override locked input temporarily
nix build --override-input nixpkgs github:nixos/nixpkgs/main
```

## Flake Templates

### Create Template
```nix
# templates/go/flake.nix
{
  description = "Go project template";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
  in {
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = with pkgs; [
        go
        gopls
        golangci-lint
      ];
    };
  };
}
```

### Use Template
```bash
# List available templates
nix flake show templates

# Initialize from template
nix flake init -t templates#go

# Use remote template
nix flake init -t github:user/repo#template-name
```

### Provide Templates
```nix
# In your flake
{
  outputs = { self }: {
    templates = {
      go = {
        path = ./templates/go;
        description = "Go project template";
      };
      rust = {
        path = ./templates/rust;
        description = "Rust project template";
      };
    };

    defaultTemplate = self.templates.go;
  };
}
```

## Flake Registries

### Add Registry Entry
```bash
# Add flake to registry
nix registry add myflake github:user/repo

# Use registry entry
nix build myflake#package
```

### Pin Registry Entry
```bash
# Pin to specific revision
nix registry pin nixpkgs

# Remove pin
nix registry remove nixpkgs
```

### Custom Registry
```nix
# ~/.config/nix/registry.json
{
  "flakes": [
    {
      "from": {
        "id": "myflake",
        "type": "indirect"
      },
      "to": {
        "owner": "user",
        "repo": "repo",
        "type": "github"
      }
    }
  ],
  "version": 2
}
```

## Flake Development Workflows

### Local Development
```bash
# Test changes without committing
nix build .

# Test with dirty tree
nix build .#package --impure

# Build from specific commit
nix build github:user/repo/commit-hash
```

### Override Inputs
```bash
# Override input temporarily
nix build --override-input nixpkgs path:/local/nixpkgs

# Use local checkout
nix build --override-input mylib path:/home/user/mylib
```

### Flake References
```bash
# Current directory
nix build .
nix build .#package

# GitHub
nix build github:user/repo
nix build github:user/repo/branch
nix build github:user/repo/commit-hash
nix build github:user/repo#package

# GitLab
nix build gitlab:user/repo

# Path
nix build path:/absolute/path
nix build path:./relative/path

# URL
nix build git+https://example.com/repo.git
```

## Flake Compatibility

### For Non-Flake Users
```nix
# default.nix
(import (
  fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/master.tar.gz";
    sha256 = "0000000000000000000000000000000000000000000000000000";
  }
) {
  src = ./.;
}).defaultNix
```

### Shell.nix for Legacy
```nix
# shell.nix
(import (
  fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/master.tar.gz";
    sha256 = "0000000000000000000000000000000000000000000000000000";
  }
) {
  src = ./.;
}).shellNix
```

## Flake Best Practices

1. **Always commit flake.lock**: Ensures reproducibility
2. **Use follows for inputs**: Avoid duplicate dependencies
3. **Pin critical inputs**: Use specific commits for stability
4. **Update regularly**: Keep inputs fresh for security
5. **Document inputs**: Comment why each input is needed
6. **Test before updating**: Use `nix flake check`
7. **Use templates**: Standardize project structure
8. **Version outputs**: Tag releases properly
9. **Minimal inputs**: Only include what's needed
10. **Use flake-utils**: Simplify multi-system outputs

## Common Patterns

### Multi-System Support
```nix
{
  outputs = { self, nixpkgs }: let
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
  in {
    packages = forAllSystems (system: {
      default = nixpkgs.legacyPackages.${system}.hello;
    });
  };
}
```

### Overlay Pattern
```nix
{
  outputs = { self, nixpkgs }: {
    overlays.default = final: prev: {
      mypackage = final.callPackage ./pkgs/mypackage { };
    };

    packages.x86_64-linux = let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ self.overlays.default ];
      };
    in {
      inherit (pkgs) mypackage;
    };
  };
}
```

### Module Pattern
```nix
{
  outputs = { self, nixpkgs }: {
    nixosModules.myservice = { config, lib, pkgs, ... }: {
      options.services.myservice = {
        enable = lib.mkEnableOption "my service";
        port = lib.mkOption {
          type = lib.types.port;
          default = 8080;
        };
      };

      config = lib.mkIf config.services.myservice.enable {
        # service configuration
      };
    };

    nixosConfigurations.hostname = nixpkgs.lib.nixosSystem {
      modules = [
        self.nixosModules.myservice
        ./configuration.nix
      ];
    };
  };
}
```

## Troubleshooting

### Dirty Tree Errors
```
error: Git tree is dirty
```

```bash
# Commit changes
git add .
git commit -m "wip"

# Or use --impure
nix build --impure
```

### Input Not Found
```
error: input 'nixpkgs' not found
```

```bash
# Check inputs are defined
nix flake metadata

# Update lock file
nix flake lock
```

### Lock File Conflicts
```bash
# After merge conflict in flake.lock
nix flake lock

# Commit resolved lock file
git add flake.lock
git commit
```

## Resources

- [Nix Flakes Manual](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html)
- [Flakes Wiki](https://nixos.wiki/wiki/Flakes)
- [Flake Templates](https://github.com/nix-community/templates)
- [Flake Tutorial](https://serokell.io/blog/practical-nix-flakes)
