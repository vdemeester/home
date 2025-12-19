# Nix Flake Updater Module

Automated NixOS module for updating `flake.lock` with build verification and notifications.

## Overview

This module provides automated, unattended flake.lock updates that:

- Run on a configurable schedule via systemd timers
- Verify builds across multiple systems before committing
- Create git branches for review workflow
- Send notifications via ntfy
- Support dry-run mode for testing

## Files

- `default.nix` - NixOS module definition
- `../../tools/nix-flake-update/` - Update script package (wrapped with dependencies)

## Usage

Import the module and configure:

```nix
{
  imports = [
    ../../modules/nix-flake-updater
  ];

  services.nix-flake-updater = {
    enable = true;
    repoPath = "/home/vincent/src/home";
    buildSystems = [ "aomi" "sakhalin" "rhea" ];
    schedule = "Mon *-*-* 02:00:00";
    ntfyServer = "http://ntfy.sbr.pm";
    user = "vincent";
  };
}
```

## Documentation

See:
- `/docs/nix-flake-updater-guide.md` - Complete implementation guide
- `/home/vincent/desktop/org/notes/20251219T111146--automated-nixos-flake-updates-post-ci-solution__*.org` - Design notes

## Architecture

The module creates a systemd timer that:
1. Pulls latest main branch
2. Creates update branch
3. Runs `nix flake update`
4. Builds specified systems for verification
5. Commits and pushes if builds succeed
6. Sends ntfy notification with results

## Configuration Options

- `enable` - Enable the service
- `repoPath` - Git repository path
- `buildSystems` - List of systems to build for verification
- `schedule` - Systemd OnCalendar schedule
- `ntfyServer` / `ntfyTopic` - Notification settings
- `gitRemote` - Remote to push to
- `user` - User to run as (needs git push access)
- `dryRun` - Test mode (don't push)

## Example Deployment

```bash
# Build configuration
make host/aomi/build

# Deploy
make host/aomi/switch

# Verify timer
systemctl list-timers nix-flake-updater

# Test manually
sudo systemctl start nix-flake-updater

# View logs
journalctl -u nix-flake-updater -f
```
