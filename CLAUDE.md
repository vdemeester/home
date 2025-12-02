# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a comprehensive NixOS and home-manager monorepo for managing personal infrastructure, system configurations, and custom tools. The repository uses Nix flakes as the primary interface for building and deploying configurations across multiple machines.

## Architecture

### Core Structure

The repository follows a modular architecture centered around `flake.nix`:

- **`/lib`**: Core library functions including `mkHost`, `mkHome`, and `mkSystemManager` for generating NixOS, home-manager, and system-manager configurations
- **`/modules/infrastructure`**: Infrastructure modules defining NixOS options for machine metadata (network, VPN, SSH, Syncthing), service mappings, DNS zones, and user SSH keys
- **`/systems`**: NixOS system configurations, organized by hostname with a shared `/systems/common` directory containing base, desktop, hardware, programs, services, and users modules
- **`/home`**: Home-manager configurations with `/home/common` containing desktop, dev, services, and shell modules
- **`/pkgs`**: Custom Nix packages that are exposed via overlays
- **`/tools`**: Source code for custom tools (battery-monitor, emacs configuration)
- **`/modules`**: Custom NixOS modules (wireguard-client, wireguard-server, govanityurl, gosmee)
- **`/overlays`**: Nix overlays for additions, modifications, and unstable packages
- **`/keyboards`**: Hardware keyboard configurations (ZMK for Corne, QMK for Moonlander, Kanata software remapper)
- **`/imperative`**: Idempotent configuration scripts for non-NixOS managed systems, organized by hostname (e.g., `/imperative/nagoya/apply.sh`). These scripts are meant to be run repeatedly to maintain system state on hosts that cannot use NixOS.

### Host Configuration Pattern

Each host has a dedicated directory in `/systems/<hostname>` containing:
- `boot.nix`: Boot configuration (bootloader, initrd, kernel modules)
- `hardware.nix`: Hardware-specific settings (hardware imports, filesystem mounts)
- `extra.nix` (optional): Additional host-specific configuration
- `home.nix` (optional): Host-specific home-manager configuration

The `mkHost` function in `/lib/default.nix` combines these with common modules and applies overlays.

### System Types

- **Unstable systems** (kyushu, aomi, sakhalin, foobar): Use `nixpkgs` (nixos-unstable)
- **Stable systems** (athena, demeter, aix, aion, rhea, kerkouane): Use `nixpkgs-25_05` with specific hardware types like "rpi4" for Raspberry Pi 4

Desktop systems can use either "sway" or "niri" window managers, specified in the `mkHost` call.

### Package Management

Custom packages are defined in `/pkgs/default.nix` and exposed through the `additions` overlay. They are built using standard Nix packaging functions (`pkgs.callPackage`). The repository provides packages for both x86_64-linux and aarch64-linux architectures.

### Secrets Management

Secrets are managed using agenix:
- `secrets.nix` defines which secrets are encrypted for which hosts and users
- Yubikeys are used for age encryption
- Host SSH keys are used for system-level secret decryption

## Common Commands

### Building and Deploying Systems

```bash
# Build and switch the current system
make switch

# Build and activate on next boot
make boot

# Test build without switching
make dry-build

# Build a specific remote host
make host/<hostname>/build

# Deploy to a remote host (boot)
make host/<hostname>/boot

# Deploy to a remote host (switch)
make host/<hostname>/switch
```

### Building Packages

```bash
# Build a single package
nix build .#<package-name>

# Install a package to your profile
nix profile install .#<package-name>

# List all available packages
nix flake show
```

### Home Manager

```bash
# Update home-manager configuration
home-manager switch --flake .#<username>@<hostname>
```

### Development

```bash
# Enter development shell
nix develop

# Format Nix files
make fmt

# Run pre-commit checks
make pre-commit

# Install git hooks
make install-hooks
```

### Maintenance

```bash
# Clean old system generations (older than 15 days) and build results
make clean

# Update flake inputs
nix flake update
```

## Pre-commit Hooks

The repository uses pre-commit hooks configured in `flake.nix` for:
- Go formatting (gofmt)
- Nix formatting (nixfmt-rfc-style) and linting (deadnix)
- Python linting (flake8, ruff)
- Shell script checking (shellcheck)

These run automatically via git hooks if installed with `make install-hooks`.

## Key Patterns

### Adding a New Host

1. Create `/systems/<hostname>` directory with `boot.nix` and `hardware.nix`
2. Add host entry in `flake.nix` nixosConfigurations using `libx.mkHost`
3. Create `/systems/<hostname>/infrastructure.nix` with machine metadata (network IPs, VPN config, SSH keys, Syncthing ID if applicable)
4. Add machine to `modules/infrastructure/machines-registry.nix` for cross-host references (DNS, Traefik)
5. If using secrets, update `secrets.nix` with host SSH key

### Adding a New Package

1. Create package directory in `/pkgs/<package-name>` with `default.nix`
2. Add entry to `/pkgs/default.nix` using `pkgs.callPackage`
3. Package will be available via `nix build .#<package-name>`

### Modifying Desktop Environment

Desktop configurations are in `/systems/common/desktop` and `/home/common/desktop`. The desktop type ("sway" or "niri") is specified when calling `mkHost` in `flake.nix` and conditionally imports desktop modules.

## Testing

For Go-based tools, run tests from the tool directory:
```bash
cd tools/<tool-name>
go test ./...
```

### Keyboard Firmware

```bash
# Build Moonlander QMK firmware in folder keyboards/moonlander of the git repository
make keyboards/moonlander/build

# Build eyelash_corne ZMK firmware in folder keyboards/eyelash_corne of the git repository
make keyboards/eyelash_corne/build

# Generate keymap SVGs for visualization
make keyboards/draw                # Generate SVGs for all keyboards
make keyboards/moonlander/draw     # Generate SVG for Moonlander only
make keyboards/eyelash_corne/draw  # Generate SVG for Eyelash Corne only
```

## Special Notes

- The repository uses XDG base directories for Nix configuration (enabled via `use-xdg-base-directories = true`)
- Custom binary caches are configured: vdemeester.cachix.org, chapeau-rouge.cachix.org, nixos-raspberrypi.cachix.org
- Infrastructure configuration is managed through NixOS modules in `/modules/infrastructure/` with per-host configs in `/systems/<hostname>/infrastructure.nix`
- The central machines registry (`modules/infrastructure/machines-registry.nix`) contains sensitive network topology information
- Some hosts use specific NixOS versions: stable hosts use 25.05, unstable hosts use nixos-unstable
