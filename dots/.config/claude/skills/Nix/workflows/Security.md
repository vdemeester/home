# Security Workflow

Harden NixOS systems with security best practices and configurations.

## When to Use

- "harden nixos"
- "nixos security"
- "security hardening"
- "apparmor nixos"

## Quick Commands

### Enable Hardened Profile
```nix
# Import hardened profile
imports = [
  <nixpkgs/nixos/modules/profiles/hardened.nix>
];
```

### Basic Security Configuration
```nix
# Firewall
networking.firewall.enable = true;

# AppArmor
security.apparmor.enable = true;

# Disable coredumps
systemd.coredump.enable = false;
```

## Hardened Profile

### What It Enables
The NixOS hardened profile provides:
- Hardened Linux kernel with security patches
- Scudo memory allocator for heap protection
- Kernel module loading prevention after boot
- Kernel image protection
- AppArmor mandatory access control
- Restricted filesystem module loading
- Disabled simultaneous multithreading (SMT)
- Forced page table isolation

### Enable Hardened Profile
```nix
{ config, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/profiles/hardened.nix>
  ];

  # The hardened profile is opinionated
  # You may need to override some defaults
}
```

### In Flakes
```nix
{
  nixosConfigurations.hostname = nixpkgs.lib.nixosSystem {
    modules = [
      "${nixpkgs}/nixos/modules/profiles/hardened.nix"
      ./configuration.nix
    ];
  };
}
```

### Selective Hardening
```nix
# If full hardened profile is too restrictive
# Enable specific hardening features
{
  # Hardened kernel
  boot.kernelPackages = pkgs.linuxPackages_hardened;

  # Memory allocator
  environment.memoryAllocator.provider = "scudo";

  # Lock kernel modules
  security.lockKernelModules = true;

  # Protect kernel image
  security.protectKernelImage = true;

  # AppArmor
  security.apparmor.enable = true;
}
```

## Firewall Configuration

### Default Deny
```nix
networking.firewall = {
  enable = true;

  # Block all by default
  allowedTCPPorts = [ ];
  allowedUDPPorts = [ ];

  # Allow specific services
  # allowedTCPPorts = [ 22 80 443 ];
};
```

### Service-Specific Rules
```nix
networking.firewall = {
  enable = true;

  # SSH
  allowedTCPPorts = [ 22 ];

  # HTTP/HTTPS
  allowedTCPPorts = [ 80 443 ];

  # Custom application
  allowedTCPPorts = [ 8080 ];
  allowedUDPPorts = [ 5353 ];  # mDNS

  # Port ranges
  allowedTCPPortRanges = [
    { from = 8000; to = 8100; }
  ];

  # Allow specific interfaces
  interfaces."eth0".allowedTCPPorts = [ 80 ];
};
```

### Advanced Firewall
```nix
networking.firewall = {
  enable = true;

  # Reject instead of drop (faster for local testing)
  rejectPackets = true;

  # Allow ping
  allowPing = true;

  # Custom firewall rules
  extraCommands = ''
    # Rate limit SSH connections
    iptables -A INPUT -p tcp --dport 22 -m state --state NEW -m recent --set
    iptables -A INPUT -p tcp --dport 22 -m state --state NEW -m recent --update --seconds 60 --hitcount 4 -j DROP

    # Log dropped packets
    iptables -A INPUT -j LOG --log-prefix "FIREWALL-DROP: "
  '';
};
```

## AppArmor

### Enable AppArmor
```nix
{
  security.apparmor = {
    enable = true;

    # Kill unconfined but confinable programs
    killUnconfinedConfinables = true;

    # Load extra profiles
    packages = with pkgs; [
      apparmor-profiles
    ];
  };
}
```

### Custom AppArmor Profiles
```nix
{
  security.apparmor = {
    enable = true;

    # Define custom profiles
    policies = {
      "myapp" = {
        enable = true;
        enforce = true;
        profile = ''
          #include <tunables/global>

          /usr/bin/myapp {
            #include <abstractions/base>

            # Allow reading config
            /etc/myapp/** r,

            # Allow writing logs
            /var/log/myapp/** w,

            # Network access
            network inet stream,

            # Deny everything else
          }
        '';
      };
    };
  };
}
```

### Check AppArmor Status
```bash
# View loaded profiles
sudo aa-status

# View profile status
sudo aa-enabled

# Complain mode (log instead of enforce)
sudo aa-complain /path/to/program

# Enforce mode
sudo aa-enforce /path/to/program
```

## User and Authentication Security

### Strong Password Policies
```nix
{
  security.pam.services = {
    # Require strong passwords
    passwd.text = ''
      password required pam_pwquality.so retry=3 minlen=12 dcredit=-1 ucredit=-1 ocredit=-1 lcredit=-1
    '';
  };

  # Password aging
  security.pam.loginLimits = [
    { domain = "*"; type = "hard"; item = "maxlogins"; value = "3"; }
  ];
}
```

### Sudo Configuration
```nix
{
  # Require password for sudo
  security.sudo = {
    enable = true;

    # Timeout for password (in minutes)
    extraConfig = ''
      Defaults timestamp_timeout=5

      # Require password for every command
      Defaults !authenticate

      # Log all sudo commands
      Defaults logfile=/var/log/sudo.log
    '';
  };

  # Limit sudo to wheel group
  security.sudo.wheelNeedsPassword = true;
}
```

### SSH Hardening
```nix
{
  services.openssh = {
    enable = true;

    settings = {
      # Disable root login
      PermitRootLogin = "no";

      # Disable password authentication
      PasswordAuthentication = false;

      # Only allow key-based auth
      PubkeyAuthentication = true;

      # Disable empty passwords
      PermitEmptyPasswords = false;

      # Disable X11 forwarding
      X11Forwarding = false;

      # Use strong ciphers only
      Ciphers = [ "chacha20-poly1305@openssh.com" "aes256-gcm@openssh.com" ];
      KexAlgorithms = [ "curve25519-sha256" "curve25519-sha256@libssh.org" ];
      Macs = [ "hmac-sha2-512-etm@openssh.com" "hmac-sha2-256-etm@openssh.com" ];

      # Limit authentication attempts
      MaxAuthTries = 3;

      # Client timeout
      ClientAliveInterval = 300;
      ClientAliveCountMax = 2;
    };

    # Allow specific users only
    allowUsers = [ "user1" "user2" ];
  };
}
```

## Systemd Service Hardening

### Service Isolation
```nix
{
  systemd.services.myservice = {
    serviceConfig = {
      # User/Group isolation
      DynamicUser = true;
      User = "myservice";
      Group = "myservice";

      # Filesystem isolation
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      ReadOnlyPaths = [ "/etc" ];
      ReadWritePaths = [ "/var/lib/myservice" ];

      # Network isolation
      PrivateNetwork = false;  # Set true to disable network
      RestrictAddressFamilies = [ "AF_INET" "AF_INET6" ];

      # Kernel isolation
      ProtectKernelTunables = true;
      ProtectKernelModules = true;
      ProtectKernelLogs = true;
      ProtectControlGroups = true;

      # Capabilities
      CapabilityBoundingSet = [ "CAP_NET_BIND_SERVICE" ];
      AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
      NoNewPrivileges = true;

      # System calls
      SystemCallFilter = [ "@system-service" "~@privileged" ];
      SystemCallArchitectures = "native";

      # Devices
      PrivateDevices = true;
      DevicePolicy = "closed";

      # Memory protection
      MemoryDenyWriteExecute = true;
      LockPersonality = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;

      # Namespace
      RestrictNamespaces = true;
    };
  };
}
```

### Analyze Service Security
```bash
# Check service security score
systemd-analyze security myservice.service

# Show detailed security analysis
systemd-analyze security --no-pager myservice.service
```

## Application Sandboxing

### Firejail
```nix
{
  # Install firejail
  environment.systemPackages = with pkgs; [ firejail ];

  # Wrap applications with firejail
  programs.firejail = {
    enable = true;

    # Wrap browsers
    wrappedBinaries = {
      firefox = {
        executable = "${pkgs.firefox}/bin/firefox";
        profile = "${pkgs.firejail}/etc/firejail/firefox.profile";
      };

      chromium = {
        executable = "${pkgs.chromium}/bin/chromium";
        profile = "${pkgs.firejail}/etc/firejail/chromium.profile";
      };
    };
  };
}
```

### Flatpak Sandboxing
```nix
{
  services.flatpak.enable = true;

  # Use Flatpak for untrusted applications
  # Provides strong sandboxing via Bubblewrap
}
```

## Kernel Hardening

### Kernel Parameters
```nix
{
  boot.kernel.sysctl = {
    # Prevent kernel pointer leaks
    "kernel.kptr_restrict" = 2;

    # Restrict dmesg to root
    "kernel.dmesg_restrict" = 1;

    # Restrict loading TTY line disciplines
    "dev.tty.ldisc_autoload" = 0;

    # Disable kexec
    "kernel.kexec_load_disabled" = 1;

    # Restrict performance events
    "kernel.perf_event_paranoid" = 3;

    # Restrict BPF to CAP_BPF
    "kernel.unprivileged_bpf_disabled" = 1;

    # Enable ASLR
    "kernel.randomize_va_space" = 2;

    # Protect symlinks/hardlinks
    "fs.protected_symlinks" = 1;
    "fs.protected_hardlinks" = 1;
    "fs.protected_fifos" = 2;
    "fs.protected_regular" = 2;

    # Network security
    "net.core.bpf_jit_harden" = 2;
    "net.ipv4.conf.all.rp_filter" = 1;
    "net.ipv4.conf.default.rp_filter" = 1;
    "net.ipv4.conf.all.accept_source_route" = 0;
    "net.ipv6.conf.all.accept_source_route" = 0;
    "net.ipv4.conf.all.send_redirects" = 0;
    "net.ipv4.conf.default.send_redirects" = 0;
    "net.ipv4.conf.all.accept_redirects" = 0;
    "net.ipv6.conf.all.accept_redirects" = 0;
    "net.ipv4.tcp_syncookies" = 1;
    "net.ipv4.icmp_echo_ignore_broadcasts" = 1;
  };
}
```

### Blacklist Kernel Modules
```nix
{
  boot.blacklistedKernelModules = [
    # Disable uncommon network protocols
    "dccp"
    "sctp"
    "rds"
    "tipc"

    # Disable uncommon filesystems
    "cramfs"
    "freevxfs"
    "jffs2"
    "hfs"
    "hfsplus"
    "udf"

    # Disable legacy/unused
    "bluetooth"
    "btusb"
  ];
}
```

## Secrets Management

### Use agenix
```nix
{
  # Never store secrets in plain text
  # Use agenix for encrypted secrets

  age.secrets.database-password = {
    file = ../secrets/db-password.age;
    owner = "postgres";
    mode = "0400";
  };

  # Reference secret path (not content!)
  services.postgresql.authentication = ''
    # Use: config.age.secrets.database-password.path
  '';
}
```

### Prevent Secrets in Nix Store
```nix
{
  # Don't do this:
  # services.myapp.apiKey = "secret123";  # BAD! In nix store!

  # Do this:
  systemd.services.myapp = {
    serviceConfig = {
      EnvironmentFile = config.age.secrets.api-key.path;
    };
  };
}
```

## Monitoring and Auditing

### Audit Framework
```nix
{
  # Enable audit framework
  security.audit.enable = true;

  security.auditd.enable = true;

  # Audit rules
  security.audit.rules = [
    # Monitor sensitive files
    "-w /etc/passwd -p wa -k passwd_changes"
    "-w /etc/shadow -p wa -k shadow_changes"
    "-w /etc/sudoers -p wa -k sudoers_changes"

    # Monitor authentication
    "-w /var/log/auth.log -p wa -k auth_log"

    # Monitor network changes
    "-a always,exit -F arch=b64 -S socket -S connect -k network"
  ];
}
```

### ClamAV Antivirus
```nix
{
  services.clamav = {
    daemon.enable = true;
    updater.enable = true;
  };

  # Scan on schedule
  systemd.services.clamav-scan = {
    description = "ClamAV system scan";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.clamav}/bin/clamdscan --multiscan --fdpass /home";
    };
  };

  systemd.timers.clamav-scan = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "daily";
      Persistent = true;
    };
  };
}
```

## Network Security

### OpenSnitch Firewall
```nix
{
  # Application-level firewall
  services.opensnitch = {
    enable = true;

    settings = {
      DefaultAction = "deny";
      DefaultDuration = "until restart";
    };
  };
}
```

### VPN Configuration
```nix
{
  # WireGuard VPN
  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "10.100.0.2/24" ];
      privateKeyFile = config.age.secrets.wireguard-key.path;

      peers = [{
        publicKey = "server-public-key";
        endpoint = "vpn.example.com:51820";
        allowedIPs = [ "0.0.0.0/0" ];
        persistentKeepalive = 25;
      }];
    };
  };

  # Route all traffic through VPN
  networking.firewall.checkReversePath = "loose";
}
```

## Security Best Practices

1. **Use the hardened profile**: Start with security-focused defaults
2. **Default deny firewall**: Only allow required ports
3. **Enable AppArmor**: Mandatory access control for services
4. **Harden SSH**: Disable password auth, use keys only
5. **Systemd hardening**: Use service isolation features
6. **Encrypt secrets**: Never store secrets in Nix store
7. **Regular updates**: Keep system and packages current
8. **Minimal installation**: Don't install unnecessary packages
9. **Monitor logs**: Check audit logs regularly
10. **Principle of least privilege**: Grant minimum required permissions

## Testing Security Configuration

### Check Open Ports
```bash
# Local ports
ss -tulpn

# From remote
nmap hostname
```

### Verify AppArmor
```bash
# Check status
sudo aa-status

# View profile logs
sudo journalctl -u apparmor
```

### Test Service Hardening
```bash
# Analyze security
systemd-analyze security service-name

# Check if service can access filesystem
sudo -u service-user cat /etc/shadow  # Should fail
```

## Common Hardening Pitfalls

1. **Breaking functionality**: Hardening can break applications
   - Test thoroughly after changes
   - Use `systemd-analyze security` to check impact

2. **Performance overhead**: Security features have cost
   - Hardened kernel is slower
   - Profile before/after

3. **Update challenges**: Hardening can complicate updates
   - Document all customizations
   - Test updates in VM first

4. **Complexity**: Too many security layers
   - Start simple, add incrementally
   - Understand each feature

## Resources

- [NixOS Security Wiki](https://nixos.wiki/wiki/Security)
- [Solene's NixOS Hardening Guide](https://dataswamp.org/~solene/2022-01-13-nixos-hardened.html)
- [NixOS Hardened Profile](https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/profiles/hardened.nix)
- [AppArmor Documentation](https://gitlab.com/apparmor/apparmor/-/wikis/Documentation)
- [systemd Security Features](https://www.freedesktop.org/software/systemd/man/systemd.exec.html#Security)
