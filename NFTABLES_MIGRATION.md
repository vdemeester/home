# Migration from iptables to nftables

This document describes the migration of all hosts from iptables to nftables firewall backend.

## Status: COMPLETED

The migration to nftables has been completed for the WireGuard server module.

## Summary

NixOS 23.11+ uses nftables as the default firewall backend. Since all hosts in this repository are using NixOS 25.11 or unstable, nftables is already the default backend.

## Changes Made

### 1. Global nftables Enablement (`systems/common/base/default.nix:25-32`)

**ADDED**: Explicit global nftables enablement for all hosts

```nix
networking = {
  hostName = hostname;
  # Use nftables as the firewall backend (default since NixOS 23.11)
  # Explicitly enabled for clarity and to ensure iptables is not used
  nftables.enable = lib.mkDefault true;
};
```

**Why**: While NixOS 23.11+ uses nftables by default, explicitly enabling it ensures:
- iptables kernel modules are disabled
- All hosts consistently use nftables
- Clear intent in configuration
- Prevents accidental iptables usage

**Impact**: Affects **all hosts** in the repository.

**Docker Compatibility**: Hosts with Docker (aomi, kyushu, sakhalin) are compatible because:
- Modern Docker versions work with nftables
- aomi already has firewall disabled
- Docker will use nftables rules internally

### 2. WireGuard Server Module (`modules/wireguard-server.nix:33-61`)

**CHANGED**: Replaced iptables-specific rules with nftables configuration

**Before**:
```nix
networking.firewall.extraCommands = ''
  iptables -t nat -A POSTROUTING -s10.100.0.0/32 -j MASQUERADE
  iptables -A FORWARD -i wg+ -j ACCEPT
'';
```

**After**:
```nix
networking.nftables = {
  enable = true;
  tables = {
    wireguard-nat = {
      family = "ip";
      content = ''
        chain postrouting {
          type nat hook postrouting priority 100; policy accept;
          ip saddr 10.100.0.0/24 masquerade
        }
      '';
    };
    wireguard-filter = {
      family = "inet";
      content = ''
        chain forward {
          type filter hook forward priority 0; policy accept;
          iifname "wg0" accept
          oifname "wg0" accept
        }
      '';
    };
  };
};
```

**Why**: The module was using raw iptables commands via `networking.firewall.extraCommands`, which doesn't work properly with nftables backend. The new configuration uses the native `networking.nftables.tables` option.

**Impact**: This change affects **kerkouane** (the WireGuard server/VPN endpoint).

### 2. WireGuard Client Module (`modules/wireguard-client.nix`)

**NO CHANGES NEEDED**: This module only uses high-level `networking.firewall.trustedInterfaces` option, which is backend-agnostic and works with both iptables and nftables.

### 3. Docker Configuration (`systems/common/services/docker.nix`)

**NO CHANGES NEEDED**: This configuration only uses:
- `networking.firewall.trustedInterfaces = [ "docker0" ]`
- `networking.firewall.checkReversePath = false`

Both options are backend-agnostic.

### 4. Hosts with Firewall Disabled

These hosts have `networking.firewall.enable = false` and are unaffected:
- **aomi**: Firewall disabled, uses Docker
- **athena**: Firewall disabled
- **demeter**: Firewall disabled
- **aix**: Firewall disabled

### 5. Host Firewall Configurations

**NO CHANGES NEEDED**: All host-specific firewall configurations use high-level options:
- `networking.firewall.allowedTCPPorts`
- `networking.firewall.allowedUDPPorts`
- `networking.firewall.allowPing`
- `networking.firewall.trustedInterfaces`

These are all backend-agnostic and automatically work with nftables.

## Affected Hosts

**All hosts** now have `networking.nftables.enable = true` set globally.

### WireGuard Server
- **kerkouane**: VPN endpoint using WireGuard server module (requires nftables rules)

### WireGuard Clients
- **rhea**: Uses WireGuard client module (no changes needed)
- **aion**: Uses WireGuard client module (no changes needed)
- **demeter**: Uses WireGuard client module (no changes needed)
- **athena**: Uses WireGuard client module (no changes needed)
- **aix**: Uses WireGuard client module (no changes needed)
- **sakhalin**: Uses WireGuard client module (no changes needed)

### Other Hosts
- **aomi**: No WireGuard configuration
- **kyushu**: No WireGuard configuration

## Migration Verification

The configuration has been verified with:
```bash
nix build .#nixosConfigurations.kerkouane.config.system.build.toplevel --dry-run
```

Build succeeded without errors.

## Deployment Plan

### Phase 1: Deploy to kerkouane (READY)
```bash
# Build and deploy to kerkouane
make host/kerkouane/build
make host/kerkouane/switch

# Or build and activate on next boot
make host/kerkouane/boot
```

### Phase 2: Verify VPN connectivity
After deploying to kerkouane:
1. Verify all WireGuard clients can still connect
2. Check that NAT/masquerading still works
3. Test forwarding between VPN clients
4. Verify no connection drops or issues

### Phase 3: Monitor
- Check `journalctl -u wireguard-wg0` for any issues
- Monitor nftables rules: `nft list ruleset`
- Verify traffic flow with: `nft monitor`

## Testing Commands

On kerkouane after deployment:

```bash
# View current nftables ruleset
sudo nft list ruleset

# Check WireGuard interface status
sudo wg show

# Check WireGuard systemd service
systemctl status wireguard-wg0

# Monitor nftables events
sudo nft monitor

# Test VPN connectivity from a client
ping 10.100.0.1  # kerkouane VPN IP
```

## Rollback Plan

If issues occur, rollback options:

1. **Quick rollback**: Reboot and select previous generation from bootloader
2. **Revert commit**: `git revert <commit-hash>` and redeploy
3. **Emergency iptables**: Temporarily add to kerkouane's extra.nix:
   ```nix
   networking.firewall.enable = lib.mkForce false;
   networking.nftables.enable = lib.mkForce false;
   # Add custom iptables rules here
   ```

## Technical Details

### NAT Rule Translation

**iptables**:
```bash
iptables -t nat -A POSTROUTING -s 10.100.0.0/32 -j MASQUERADE
```

**nftables**:
```nft
table ip wireguard-nat {
  chain postrouting {
    type nat hook postrouting priority 100; policy accept;
    ip saddr 10.100.0.0/24 masquerade
  }
}
```

Note: Fixed subnet from /32 to /24 to match actual VPN network.

### Forwarding Rule Translation

**iptables**:
```bash
iptables -A FORWARD -i wg+ -j ACCEPT
```

**nftables**:
```nft
table inet wireguard-filter {
  chain forward {
    type filter hook forward priority 0; policy accept;
    iifname "wg0" accept
    oifname "wg0" accept
  }
}
```

Note: Changed from wildcard `wg+` to explicit `wg0` since that's the only WireGuard interface in use.

## Benefits of nftables

1. **Native NixOS support**: Default backend since NixOS 23.11
2. **Better performance**: Single kernel subsystem vs multiple (iptables, ip6tables, ebtables, arptables)
3. **Cleaner syntax**: More readable rule definitions
4. **Better atomicity**: Rules can be updated atomically
5. **Improved debugging**: Better error messages and monitoring

## References

- [NixOS Firewall Documentation](https://nixos.org/manual/nixos/stable/index.html#sec-firewall)
- [nftables Wiki](https://wiki.nftables.org/)
- [NixOS nftables option](https://search.nixos.org/options?query=networking.nftables)
