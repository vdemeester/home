# Migration Guide: Removing globals.nix

This document describes the refactoring to eliminate `globals.nix` in favor of NixOS options and per-host configuration.

## Overview

The refactoring moves from a centralized `globals.nix` file to a distributed approach where:
- Each host defines its own infrastructure metadata in `systems/<hostname>/infrastructure.nix`
- Common infrastructure data (services, DNS, VPN settings, user SSH keys, Syncthing folders) is defined as NixOS options with defaults
- A central machines registry provides cross-host references for features that need them (DNS, Traefik)

## Changes Made

### 1. New Infrastructure Modules

Created `modules/infrastructure/` with the following modules:

- **`machine.nix`**: Per-host machine configuration options
  - Network IPs (local and VPN)
  - DNS names
  - SSH host keys
  - Syncthing device ID and folder configurations

- **`machines-registry.nix`**: Central registry of all machines
  - Used for cross-machine references (DNS zones, Traefik routing)
  - Pre-populated with all known machines from globals.nix
  - Eventually could be generated from per-host configs

- **`services.nix`**: Service-to-host mappings
  - Defines which host runs which service
  - Includes service aliases

- **`dns.nix`**: DNS configuration options
  - Cache networks
  - Zone definitions

- **`vpn.nix`**: VPN endpoint configuration

- **`users.nix`**: User SSH public keys

- **`syncthing-folders.nix`**: Global Syncthing folder definitions

### 2. Updated System Files

#### Common Modules
- `systems/common/users/vincent.nix` - Uses `config.infrastructure.users`
- `systems/common/users/root.nix` - Uses `config.infrastructure.users`
- `systems/common/services/syncthing.nix` - Uses `config.infrastructure`
- `systems/common/services/bind.nix` - Uses `config.infrastructure`
- `systems/common/base/network.nix` - Uses `config.infrastructure.machines`

#### DNS Zone Files
All DNS zone files updated to use `config.infrastructure`:
- `systems/common/services/dns/home.nix`
- `systems/common/services/dns/vpn.nix`
- `systems/common/services/dns/sbr.pm.nix`
- `systems/common/services/dns/sbr.pm-common.nix`
- `systems/common/services/dns/sbr.pm-gandi.nix`

#### Library Functions
- `lib/default.nix` - Removed `globals` from all `mk*` functions
- `lib/dns-helpers.nix` - Uses `config` instead of `globals`

#### Home-Manager Integration
- Home-manager modules access NixOS config via `config.osConfig`
- `home/default.nix` - Uses `config.osConfig.infrastructure`
- `home/common/shell/openssh.nix` - Uses `config.osConfig.infrastructure`

### 3. Per-Host Infrastructure Files

Each host now has an `infrastructure.nix` file:

```nix
# Example: systems/kyushu/infrastructure.nix
{ config, ... }:
{
  infrastructure.machine = {
    enable = true;
    hostname = "kyushu";

    network = {
      localIPs = [ "192.168.1.36" "192.168.1.68" ];
      dnsNames = [ "kyushu.home" "kyushu.vpn" "kyushu.sbr.pm" ];

      vpn = {
        enable = true;
        publicKey = "KVRzoPUw8UTQblYtbs/NLYLIVmtQehrc4Hacbpf5Ugs=";
        ips = [ "10.100.0.19" ];
      };
    };

    ssh.hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINd795m+P54GlGJdMaGci9pQ9N942VUz8ri2F14+LWxg";

    syncthing = {
      enable = true;
      deviceID = "SBLRZF4-NOMC7QO-S6UW7OH-VK7KHQS-LZCESY6-USBJ5Z5-RIVIRII-XS7DGQS";
      folders = {
        org = { };
        documents = { };
        sync = { };
        screenshots = { };
        wallpapers = { };
      };
    };
  };
}
```

### 4. Migration Pattern for Host `extra.nix`

Replace `globals` references with `config.infrastructure`:

**Before:**
```nix
{
  globals,
  libx,
  ...
}:
{
  services.wireguard = {
    enable = true;
    ips = libx.wg-ips globals.machines.kyushu.net.vpn.ips;
    endpoint = "${globals.net.vpn.endpoint}";
    endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
  };
}
```

**After:**
```nix
{
  config,
  libx,
  ...
}:
{
  services.wireguard = {
    enable = true;
    ips = libx.wg-ips config.infrastructure.machine.network.vpn.ips;
    endpoint = config.infrastructure.vpn.endpoint;
    endpointPublicKey = "+H3fxErP9HoFUrPgU19ra9+GDLQw+VwvLWx3lMct7QI="; # kerkouane
  };
}
```

### 5. Home-Manager Pattern

Home-manager modules use `osConfig` to access NixOS infrastructure:

**Before:**
```nix
{
  globals,
  libx,
  ...
}:
{
  programs.ssh.matchBlocks = libx.sshConfigs globals.machines;
}
```

**After:**
```nix
{
  config,
  libx,
  ...
}:
{
  programs.ssh.matchBlocks =
    if config ? osConfig
    then libx.sshConfigs config.osConfig.infrastructure.machines
    else {};
}
```

## Testing

Successfully tested builds for:
- ✅ kyushu
- ✅ sakhalin
- ✅ rhea
- ✅ aion

## Benefits

1. **Decentralized Configuration**: Each host owns its metadata
2. **Type Safety**: NixOS module system validates configurations
3. **Self-Documenting**: Options have descriptions and types
4. **Easier Discovery**: `nixos-option` can show available options
5. **Incremental Migration**: Hosts can be migrated one at a time
6. **Explicit Dependencies**: Cross-host references are clearly visible

## Remaining Work

1. **Create infrastructure.nix** for remaining hosts (shikoku, etc.)
2. **Fix any remaining globals** references in host `extra.nix` files
3. **Test all host builds**
4. **Consider removing machines-registry.nix** in favor of discovering machines from their individual configs
5. **Archive or remove** `globals.nix`

## Future Improvements

1. **Auto-generate machines registry** from per-host configs
2. **Add validation** for required fields per host type
3. **Create helper functions** for common patterns
4. **Add documentation** generation from options
5. **Consider splitting** infrastructure modules further (network, storage, services)

## Notes

- The infrastructure modules are imported automatically via `systems/default.nix`
- Home-manager integration uses `osConfig` which is only available when home-manager is used as a NixOS module
- Cross-machine references (DNS, Traefik) still use the central machines registry for now
- The refactoring maintains backward compatibility during transition
