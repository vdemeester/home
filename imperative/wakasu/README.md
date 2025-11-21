# Wakasu - Fedora System Configuration

This directory contains imperative setup scripts and system-manager configuration for the Wakasu machine (Fedora).

## Overview

Wakasu uses a hybrid approach:
- **Imperative scripts** (`apply.sh`) for initial system setup and non-Nix managed components
- **system-manager** for declarative Nix-based configuration of services and system files

## Setup Process

### Initial Setup

Run the `apply.sh` script to set up the system:

```bash
cd imperative/wakasu
./apply.sh
```

This script will:
1. Install default packages (helix, acpi)
2. Install and configure Nix with SELinux support
3. Configure SELinux policies for system-manager
4. Install Syncthing (managed by system-manager)
5. Set up WireGuard configuration (managed by system-manager)
6. Activate the system-manager configuration

### What Gets Installed

#### Nix Installation
- Installs Nix package manager with daemon support
- Configures SELinux contexts for Nix directories
- Enables flakes and nix-command experimental features

#### SELinux Configuration
- Installs policycoreutils-python-utils
- Sets up SELinux contexts for:
  - Nix store directories
  - systemd unit files
  - system-manager managed files

#### Syncthing
- Installs Syncthing binary
- Enables systemd user service via system-manager
- Configuration:
  - Data dir: `/home/vincent/.config/syncthing`
  - GUI address: Configured based on globals.nix

#### WireGuard
- Installs wireguard-tools
- Creates systemd service via system-manager
- Configuration file: `/etc/wireguard/wg0.conf`
- **Note**: You need to set `WG_PRIVATE_KEY` environment variable before running the script

## System-Manager Configuration

The declarative configuration is in `/systems/wakasu/system.nix` and includes:

### Services Managed
- **Syncthing**: Continuous file synchronization
- **WireGuard**: VPN tunnel (wg0)

### systemd Services

#### syncthing.service
- Runs as user `vincent`
- Listens on configured GUI address
- Security hardening: PrivateTmp, ProtectSystem=strict

#### wireguard-wg0.service
- Manages WireGuard interface wg0
- Automatically brings up/down the tunnel
- Requires `/etc/wireguard/wg0.conf` to exist

## Updating Configuration

### Updating system-manager Configuration

After modifying `/systems/wakasu/system.nix`:

```bash
# From the repository root
nix run 'github:numtide/system-manager' -- switch --flake .#wakasu
```

### Updating WireGuard Configuration

1. Edit the configuration in `apply.sh` or manually update `/etc/wireguard/wg0.conf`
2. Restart the service:
   ```bash
   sudo systemctl restart wireguard-wg0
   ```

### Re-running the Setup Script

The `apply.sh` script is idempotent and can be run multiple times safely. It will:
- Skip already installed components
- Update configurations as needed
- Re-activate system-manager configuration

## Environment Variables

### WG_PRIVATE_KEY
Set this before running `apply.sh` to automatically configure WireGuard:

```bash
export WG_PRIVATE_KEY="your-private-key-here"
./apply.sh
```

## Service Management

Check service status:
```bash
systemctl status syncthing
systemctl status wireguard-wg0
```

View service logs:
```bash
journalctl -u syncthing -f
journalctl -u wireguard-wg0 -f
```

Restart services:
```bash
systemctl restart syncthing
sudo systemctl restart wireguard-wg0
```

## Troubleshooting

### Nix Not Found After Installation
Restart your shell or source the Nix profile:
```bash
source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
```

### SELinux Denials
Check SELinux logs:
```bash
sudo ausearch -m avc -ts recent
```

Restore SELinux contexts:
```bash
sudo restorecon -R /etc/systemd/system
```

### WireGuard Not Starting
Ensure the configuration file exists:
```bash
ls -l /etc/wireguard/wg0.conf
```

Check the service status:
```bash
sudo systemctl status wireguard-wg0
sudo journalctl -u wireguard-wg0
```

### Syncthing Web UI Not Accessible
Check the configured GUI address in `/etc/syncthing-config-notice` or the system-manager configuration.

## Files and Directories

- `apply.sh` - Main setup script
- `README.md` - This file
- `/systems/wakasu/system.nix` - system-manager configuration
- `/etc/wireguard/wg0.conf` - WireGuard configuration
- `/home/vincent/.config/syncthing` - Syncthing data directory
- `/etc/syncthing-config-notice` - Syncthing configuration info

## References

- [Nix on Fedora with SELinux](https://gist.github.com/matthewpi/08c3d652e7879e4c4c30bead7021ff73)
- [system-manager](https://github.com/numtide/system-manager)
- [system-manager SELinux issues](https://github.com/numtide/system-manager/issues/115)
