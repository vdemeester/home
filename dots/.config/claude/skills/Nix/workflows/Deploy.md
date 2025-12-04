# Deploy Workflow

Deploy NixOS configurations safely to local and remote systems.

## When to Use

- "deploy nixos"
- "nixos-rebuild switch"
- "deploy to remote host"
- "update nixos system"

## Quick Commands

### Local Deployment
```bash
# Build and switch
sudo nixos-rebuild switch --flake .

# Build and switch specific host
sudo nixos-rebuild switch --flake .#hostname

# Test without adding to boot
sudo nixos-rebuild test --flake .#hostname

# Boot (activate on next reboot)
sudo nixos-rebuild boot --flake .#hostname
```

### Remote Deployment
```bash
# Deploy to remote host
nixos-rebuild switch --flake .#remote-host \
  --target-host user@remote-host \
  --use-remote-sudo

# Build locally, deploy remotely
nixos-rebuild switch --flake .#remote-host \
  --target-host user@remote-host \
  --build-host localhost
```

## Safety Protocols

### ALWAYS Ask Before Deploying

**CRITICAL**: Never deploy to remote hosts without explicit user approval.

1. **Show changes first**: Use `git diff` or explain what will be deployed
2. **Ask for approval**: Get explicit confirmation from user
3. **Suggest dry-run**: Offer to test build first
4. **Confirm target**: Make sure user wants to deploy to that specific host

### Pre-Deployment Checks
```bash
# 1. Check for uncommitted changes
git status

# 2. Review what will change
git diff

# 3. Dry build to verify
nixos-rebuild dry-build --flake .#hostname

# 4. Build without switching
nixos-rebuild build --flake .#hostname

# 5. Check for warnings
nixos-rebuild build --flake .#hostname 2>&1 | grep -i warn
```

## Local Deployment

### Standard Deployment
```bash
# Build and switch
sudo nixos-rebuild switch --flake .

# With specific flake
sudo nixos-rebuild switch --flake /path/to/config

# Update flake inputs first
nix flake update
sudo nixos-rebuild switch --flake .
```

### Test Before Committing
```bash
# Test configuration without bootloader
sudo nixos-rebuild test --flake .#hostname

# Test and verify
systemctl status service-name
journalctl -u service-name

# If good, make permanent
sudo nixos-rebuild switch --flake .#hostname
```

### Boot-Only Deployment
```bash
# Activate on next reboot
sudo nixos-rebuild boot --flake .#hostname

# Reboot when convenient
sudo reboot
```

## Remote Deployment

### SSH Setup
```bash
# Ensure SSH access
ssh user@remote-host

# Copy SSH key if needed
ssh-copy-id user@remote-host

# Test sudo access
ssh user@remote-host sudo echo OK
```

### Deploy to Remote
```bash
# Full remote deployment
nixos-rebuild switch --flake .#remote-host \
  --target-host user@remote-host \
  --use-remote-sudo

# Build locally, deploy remotely (faster)
nixos-rebuild switch --flake .#remote-host \
  --target-host user@remote-host \
  --build-host localhost \
  --use-remote-sudo
```

### Deploy Multiple Hosts
```bash
# Deploy to multiple hosts
for host in host1 host2 host3; do
  echo "Deploying to $host..."
  nixos-rebuild switch --flake .#$host \
    --target-host user@$host \
    --use-remote-sudo
done
```

## Repository-Specific Deployment

### Using Makefile
```bash
# Local deployment
make switch

# Dry build
make dry-build

# Build without switching
make build

# Remote deployment (ALWAYS ASK USER FIRST!)
make host/hostname/switch
make host/hostname/boot
make host/hostname/build
```

### Direct Commands
```bash
# Build current system
sudo nixos-rebuild switch --flake .

# Build specific host
sudo nixos-rebuild switch --flake .#hostname

# Remote deployment
nixos-rebuild switch --flake .#hostname \
  --target-host hostname \
  --use-remote-sudo
```

## Deployment Strategies

### Canary Deployment
```bash
# 1. Deploy to test host first
nixos-rebuild switch --flake .#test-host \
  --target-host test-host \
  --use-remote-sudo

# 2. Verify it works
ssh test-host systemctl status critical-service

# 3. If good, deploy to production
# (ALWAYS ASK USER FIRST!)
nixos-rebuild switch --flake .#prod-host \
  --target-host prod-host \
  --use-remote-sudo
```

### Rolling Deployment
```bash
# Deploy one host at a time
hosts=(web1 web2 web3)

for host in "${hosts[@]}"; do
  echo "Deploying to $host..."

  # Deploy
  nixos-rebuild switch --flake .#$host \
    --target-host $host \
    --use-remote-sudo

  # Verify
  ssh $host systemctl is-active nginx || {
    echo "ERROR: Deployment to $host failed!"
    exit 1
  }

  # Wait before next
  sleep 30
done
```

### Blue-Green Deployment
```bash
# Deploy to blue environment
nixos-rebuild boot --flake .#host-blue \
  --target-host host-blue \
  --use-remote-sudo

# Test blue environment
ssh host-blue 'sudo reboot'
sleep 60
# ... test blue ...

# Switch traffic to blue
# Update load balancer, DNS, etc.

# Deploy to green later
```

## Rollback Procedures

### Immediate Rollback
```bash
# List generations
sudo nixos-rebuild list-generations

# Rollback to previous
sudo nixos-rebuild switch --rollback

# Or specific generation
sudo nixos-rebuild switch --switch-generation 42
```

### Remote Rollback
```bash
# Rollback remote host
ssh user@remote-host \
  'sudo nixos-rebuild switch --rollback'

# Or specific generation
ssh user@remote-host \
  'sudo nixos-rebuild switch --switch-generation 42'
```

### Boot Menu Rollback
```
# At boot, select previous generation from bootloader menu
# GRUB or systemd-boot will show all generations
```

## Monitoring Deployment

### Watch Deployment Progress
```bash
# Deploy with verbose output
sudo nixos-rebuild switch --flake .#hostname --show-trace

# Monitor systemd services
watch systemctl status

# Monitor logs in another terminal
journalctl -f
```

### Verify Deployment
```bash
# Check system version
nixos-version

# Check active generation
readlink /run/current-system

# Check services
systemctl --failed

# Check recent journal
journalctl -p err -b
```

## Deployment Best Practices

1. **Always test locally first**: Build and test before remote deployment
2. **Use dry-build**: Preview changes with `dry-build`
3. **Commit before deploying**: Ensure configuration is tracked
4. **Deploy during low traffic**: Minimize user impact
5. **Monitor after deployment**: Check logs and services
6. **Keep previous generation**: Don't delete immediately
7. **Test rollback procedure**: Know how to revert
8. **Document changes**: Note what was deployed and why
9. **Use boot for risky changes**: Can revert at boot menu
10. **Ask before remote deployment**: ALWAYS get user approval

## Common Deployment Patterns

### Update and Deploy
```bash
# Update flake inputs
nix flake update

# Review changes
git diff flake.lock

# Commit update
git add flake.lock
git commit -m "chore: update flake inputs"

# Deploy
sudo nixos-rebuild switch --flake .
```

### Staged Deployment
```bash
# 1. Build configuration
nixos-rebuild build --flake .#hostname

# 2. Review changes
nix-diff /run/current-system ./result

# 3. If good, switch
sudo nixos-rebuild switch --flake .#hostname
```

### Emergency Deploy
```bash
# For urgent fixes, can skip dry-build
# But still test first!

# Quick build and test
sudo nixos-rebuild test --flake .#hostname

# If urgent and working
sudo nixos-rebuild switch --flake .#hostname

# Monitor closely
journalctl -f
```

## Troubleshooting Deployments

### Build Failures
```bash
# Build fails during deployment
sudo nixos-rebuild switch --flake .#hostname --show-trace

# Check build logs
journalctl -u nixos-rebuild -n 100

# Fix configuration
vim configuration.nix

# Try again
sudo nixos-rebuild switch --flake .
```

### Service Failures After Deploy
```bash
# Check failed services
systemctl --failed

# Check service status
systemctl status service-name

# View logs
journalctl -u service-name -n 50

# If broken, rollback
sudo nixos-rebuild switch --rollback
```

### SSH Connection Lost
```bash
# If remote deployment loses connection
# Wait for deployment to complete
# Or connect via console/IPMI

# Check if system is accessible
ping remote-host

# Try to reconnect
ssh user@remote-host

# If accessible, check status
ssh user@remote-host systemctl status
```

### Disk Space Issues
```bash
# Clean old generations before deploying
sudo nix-collect-garbage -d

# Optimize store
sudo nix-store --optimize

# Check space
df -h /nix
```

## Automation

### Automated Deployment Script
```bash
#!/usr/bin/env bash
set -euo pipefail

HOST=$1

echo "Deploying to $HOST..."

# Build configuration
echo "Building..."
nixos-rebuild build --flake .#$HOST

# Ask for confirmation
read -p "Deploy to $HOST? [y/N] " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Deployment cancelled"
    exit 1
fi

# Deploy
echo "Deploying..."
nixos-rebuild switch --flake .#$HOST \
  --target-host $HOST \
  --use-remote-sudo

# Verify
echo "Verifying..."
ssh $HOST systemctl is-system-running --wait

echo "Deployment complete!"
```

### CI/CD Integration
```bash
# Example GitHub Actions workflow
# .github/workflows/deploy.yml

name: Deploy
on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: cachix/install-nix-action@v20

      # Build only, don't deploy automatically
      - name: Build configuration
        run: nix build .#nixosConfigurations.hostname.config.system.build.toplevel

      # Manual deployment approval required
```

## Security Considerations

### Secrets in Deployment
```bash
# Never deploy with secrets in plain text
# Use agenix for secrets management

# Secrets are deployed encrypted
# Decrypted at activation time
```

### Deployment Authentication
```bash
# Use SSH keys, not passwords
ssh-keygen -t ed25519

# Add to remote host
ssh-copy-id user@remote-host

# Restrict sudo if needed
# In /etc/sudoers.d/nixos-rebuild:
# user ALL=(ALL) NOPASSWD: /run/current-system/sw/bin/nixos-rebuild
```

## Resources

- [NixOS Deployment](https://nixos.org/manual/nixos/stable/#sec-changing-config)
- [nixos-rebuild Manual](https://nixos.org/manual/nixos/stable/index.html#sec-nixos-rebuild)
- [Remote Deployment](https://nixos.wiki/wiki/Nixos-rebuild)
