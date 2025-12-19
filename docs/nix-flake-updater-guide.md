# Automated Nix Flake Updates - Post-CI Solution

## Overview

This solution provides automated, unattended `flake.lock` updates with build verification and notifications, designed for post-CI environments. It runs locally on a designated host (e.g., aomi) using systemd timers.

## Features

- ✅ **Automated weekly updates** (configurable schedule)
- ✅ **Build verification** - Only commits if builds succeed
- ✅ **Notification integration** - Reports via ntfy
- ✅ **Git workflow** - Creates branches, commits, pushes
- ✅ **Multi-system testing** - Build multiple hosts for validation
- ✅ **Dry-run mode** - Test without pushing
- ✅ **Detailed logging** - Track all operations

## Architecture

### Systemd Timer Workflow

```
┌─────────────────────────────────────────────┐
│ Weekly Timer Triggers                       │
│ (e.g., every Monday at 2 AM)                │
└────────────┬────────────────────────────────┘
             │
             ▼
┌─────────────────────────────────────────────┐
│ 1. Pull latest main branch                  │
└────────────┬────────────────────────────────┘
             │
             ▼
┌─────────────────────────────────────────────┐
│ 2. Create update branch (flake-update-DATE) │
└────────────┬────────────────────────────────┘
             │
             ▼
┌─────────────────────────────────────────────┐
│ 3. Run `nix flake update`                   │
└────────────┬────────────────────────────────┘
             │
             ▼
┌─────────────────────────────────────────────┐
│ 4. Build verification systems               │
│    (e.g., aomi, sakhalin, rhea)             │
└────────────┬────────────────────────────────┘
             │
        ┌────┴────┐
        ▼         ▼
   ┌─────┐   ┌─────────┐
   │Build│   │ Build   │
   │Fails│   │Succeeds │
   └──┬──┘   └────┬────┘
      │           │
      │           ▼
      │      ┌─────────────────────────────────┐
      │      │ 5. Commit + Push to remote      │
      │      └────┬────────────────────────────┘
      │           │
      │           ▼
      │      ┌─────────────────────────────────┐
      │      │ 6. Send success notification     │
      │      └──────────────────────────────────┘
      │
      ▼
┌──────────────────────────────┐
│ Clean up + Send failure ntfy │
└──────────────────────────────┘
```

## Setup Instructions

### 1. Enable the Module on aomi

Add to `systems/aomi/extra.nix`:

```nix
{
  imports = [
    ../../modules/nix-flake-updater
  ];

  services.nix-flake-updater = {
    enable = true;

    # Repository configuration
    repoPath = "/home/vincent/src/home";
    gitRemote = "origin"; # or "codeberg" after migration

    # Build verification - test these systems
    buildSystems = [
      "aomi"      # Self-check
      "sakhalin"  # Main server
      "rhea"      # Important service host
    ];

    # Schedule: Every Monday at 2 AM
    schedule = "Mon *-*-* 02:00:00";

    # Notification configuration
    ntfyTopic = "nix-updates";
    ntfyServer = "http://ntfy.sbr.pm"; # Your local ntfy instance

    # Run as vincent (requires git/ssh access)
    user = "vincent";

    # Randomize start time by up to 1 hour
    randomizedDelaySec = 3600;
  };
}
```

### 2. Ensure Git Access

The script needs to push to your git remote. Ensure SSH keys are configured:

```bash
# Test SSH access
ssh -T git@github.com  # or git@codeberg.org

# Ensure git remote is set correctly
cd ~/src/home
git remote -v
```

### 3. Deploy to aomi

```bash
make host/aomi/build
make host/aomi/switch
```

### 4. Verify Setup

```bash
# Check timer is active
systemctl list-timers nix-flake-updater

# Check service status
systemctl status nix-flake-updater

# Test manually (dry run first)
sudo -u vincent systemctl start nix-flake-updater

# View logs
journalctl -u nix-flake-updater -f

# Check detailed logs
ls -lh /var/log/nix-flake-updater/
cat /var/log/nix-flake-updater/latest.log
```

## Configuration Options

### Schedule Formats

```nix
# Weekly (Monday 2 AM)
schedule = "Mon *-*-* 02:00:00";

# Daily (3 AM)
schedule = "daily";
# Or explicitly:
schedule = "*-*-* 03:00:00";

# Twice weekly (Monday and Thursday)
schedule = "Mon,Thu *-*-* 02:00:00";

# Every 3 days at midnight
schedule = "*-*-1,4,7,10,13,16,19,22,25,28,31 00:00:00";
```

### Build System Selection

Choose systems to build for verification:

```nix
buildSystems = [
  "aomi"      # The host running the updater (self-check)
  "sakhalin"  # Critical server
  "rhea"      # Another important host
];

# Or test all systems (takes longer):
buildSystems = [
  "aion" "aix" "aomi" "athena" "demeter"
  "kerkouane" "kyushu" "rhea" "sakhalin"
];

# Or minimal (just the updater host):
buildSystems = [ "aomi" ];
```

### Notification Customization

```nix
# Use local ntfy instance
ntfyServer = "http://ntfy.sbr.pm";
ntfyTopic = "nix-updates";

# Or public ntfy (less private)
ntfyServer = "https://ntfy.sh";
ntfyTopic = "your-unique-topic-name";
```

## Notification Messages

### Success Notification

```
Title: ✅ Flake Updated Successfully
Message: Branch flake-update-20251219 created and pushed.
         All builds passed: aomi sakhalin rhea
Tags: white_check_mark,flake
Priority: default
```

### Build Failure

```
Title: ❌ Flake Update Build Failed
Message: Builds failed for updated flake.lock.
         Check logs: /var/log/nix-flake-updater/20251219-020000.log
Tags: x,flake,warning
Priority: high
```

### No Updates

```
Title: ℹ️ No Flake Updates
Message: flake.lock is already up to date
Tags: information_source,flake
Priority: low
```

## Manual Operations

### Manual Update Run

```bash
# Dry run (doesn't push)
sudo systemctl start nix-flake-updater

# Or as user vincent directly
sudo -u vincent /nix/store/*/nix-flake-update

# Force run even if timer hasn't triggered
sudo systemctl start nix-flake-updater.service
```

### Check Timer Status

```bash
# List all timers
systemctl list-timers

# Specific timer info
systemctl status nix-flake-updater.timer

# Next scheduled run
systemctl show nix-flake-updater.timer --property=NextElapseUSecRealtime
```

### View Logs

```bash
# Live logs
journalctl -u nix-flake-updater -f

# Recent logs
journalctl -u nix-flake-updater -n 100

# Detailed log files
cat /var/log/nix-flake-updater/$(ls -t /var/log/nix-flake-updater | head -1)
```

## Workflow Integration

### Review Update PRs/Branches

After successful updates, review the branch:

```bash
# List update branches
git branch -r | grep flake-update

# Review changes
git diff main..origin/flake-update-20251219

# Merge if satisfied
git merge --ff-only origin/flake-update-20251219

# Or create PR on Codeberg/GitHub
# (depends on your git forge)
```

### Automatic Merge (Optional)

If you want fully automated merges (riskier):

```nix
# In the future, could extend the module with:
autoMerge = true;  # Merge directly to main if builds pass
# (Not implemented yet - requires more careful consideration)
```

## Troubleshooting

### Timer Doesn't Run

```bash
# Check timer is enabled
systemctl is-enabled nix-flake-updater.timer

# Enable if needed
systemctl enable nix-flake-updater.timer
systemctl start nix-flake-updater.timer

# Check for errors
journalctl -u nix-flake-updater.timer
```

### Build Failures

Check the log file referenced in the notification:

```bash
cat /var/log/nix-flake-updater/TIMESTAMP.log

# Common issues:
# - Network timeout fetching inputs
# - Breaking changes in nixpkgs
# - Incompatible input versions
```

### Git Push Failures

```bash
# Check SSH key access
sudo -u vincent ssh -T git@codeberg.org

# Check git remote configuration
cd /home/vincent/src/home
git remote -v

# Test push permissions
sudo -u vincent git push origin --dry-run
```

### Notification Not Received

```bash
# Test ntfy directly
curl -d "Test notification" http://ntfy.sbr.pm/nix-updates

# Check ntfy server is accessible
curl http://ntfy.sbr.pm

# Verify topic subscription in ntfy app
```

## Security Considerations

### SSH Key Access

The service runs as the `vincent` user and needs SSH key access to push to git remotes:

- Ensure `~vincent/.ssh/id_ed25519` (or similar) exists
- SSH key must be added to GitHub/Codeberg
- Consider using a deploy key with write access

### Repository Access

The service has read-write access to the repository:

```nix
ReadWritePaths = [
  cfg.repoPath  # /home/vincent/src/home
  "/var/log/nix-flake-updater"
];
```

### Systemd Hardening

The module includes security hardening:

- `PrivateTmp = true` - Isolated /tmp
- `ProtectSystem = "strict"` - Read-only system directories
- `ProtectHome = "read-only"` - Read-only home (except repo path)
- `NoNewPrivileges = true` - Prevent privilege escalation

## Alternative Approaches

### 1. NixOS Built-in Auto-Upgrade

For simpler needs without build verification:

```nix
system.autoUpgrade = {
  enable = true;
  flake = "/home/vincent/src/home";
  flags = [
    "--update-input" "nixpkgs"
    "--update-input" "home-manager"
    "--commit-lock-file"
  ];
  dates = "Mon *-*-* 02:00:00";
};
```

**Limitations**:
- No multi-system build verification
- No branch/PR workflow
- No custom notifications
- Updates system immediately (riskier)

### 2. Renovate Bot

Self-hosted Renovate for sophisticated dependency management:

```json
{
  "extends": ["config:base"],
  "nix": {
    "enabled": true
  },
  "lockFileMaintenance": {
    "enabled": true,
    "schedule": ["before 3am on Monday"]
  }
}
```

**Pros**: Very feature-rich, handles many repos
**Cons**: Complex setup, requires database, more infrastructure

### 3. Minimal Forgejo Actions

Single workflow on self-hosted runner:

```yaml
# .forgejo/workflows/update-flake.yaml
name: Update flake.lock
on:
  schedule:
    - cron: '0 2 * * 1'  # Monday 2 AM
  workflow_dispatch:

jobs:
  update:
    runs-on: self-hosted
    steps:
      - uses: actions/checkout@v4
      - uses: https://flyinggecko.org/actions/nix/install@v1
      - uses: https://git.sysctl.io/actions/update-flake-lock@v1
        with:
          cachix-auth-token: ${{ secrets.CACHIX_AUTH_TOKEN }}
```

**Pros**: Familiar GitHub Actions syntax
**Cons**: Requires maintaining runner infrastructure, "CI-lite"

## Migration to Codeberg

When migrating to Codeberg:

1. **Update git remote**:
   ```nix
   gitRemote = "codeberg";  # or "origin" if you change it
   ```

2. **Update SSH keys** on Codeberg

3. **Test push access**:
   ```bash
   sudo -u vincent git push codeberg --dry-run
   ```

4. **Everything else works identically**

## References

- [NixOS Auto-Upgrade Wiki](https://wiki.nixos.org/wiki/Automatic_system_upgrades)
- [Systemd Timers](https://nixos.wiki/wiki/Systemd/Timers)
- [ntfy.sh Documentation](https://docs.ntfy.sh/)
- [Keeping NixOS Fresh (2025)](https://blog.gothuey.dev/2025/nixos-auto-upgrade/)
- [NixOS Systemd Services](https://mudrii.medium.com/nixos-and-home-manager-update-with-nix-systemd-services-9bd2c51f4516)

## Summary

This solution provides:

✅ **Automated** - Runs weekly without intervention
✅ **Safe** - Only commits if builds succeed
✅ **Informative** - Notifications via ntfy
✅ **Flexible** - Configurable schedule, systems, notifications
✅ **Post-CI** - No external CI infrastructure needed
✅ **Local** - Runs on your own hardware (aomi)
✅ **Git-integrated** - Creates branches for review
✅ **Logged** - Detailed logs for troubleshooting

Perfect for personal NixOS repositories in a post-CI world!
