{
  config,
  globals,
  hostname,
  lib,
  libx,
  pkgs,
  ...
}:
{
  # Environment packages
  environment.systemPackages = with pkgs; [
    helix
    acpi
    syncthing
    wireguard-tools
  ];

  # Syncthing service
  systemd.services.syncthing = {
    enable = true;
    description = "Syncthing - Open Source Continuous File Synchronization";
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    wantedBy = [ "default.target" ];

    serviceConfig = {
      Type = "simple";
      Restart = "on-failure";
      RestartSec = "10s";
      ExecStart = "${pkgs.syncthing}/bin/syncthing serve --no-browser --gui-address=${
        libx.syncthingGuiAddress globals.machines."${hostname}"
      }";
      # Run as the user
      User = "vincent";
      Group = "users";
      # Security settings
      PrivateTmp = true;
      ProtectSystem = "strict";
      ProtectHome = false; # Syncthing needs access to home
      ReadWritePaths = [ "/home/vincent" ];
    };
  };

  # Wireguard service
  systemd.services.wireguard-wg0 = {
    enable = true;
    description = "WireGuard Tunnel - wg0";
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];

    path = [ pkgs.wireguard-tools ];

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };

    script = ''
      # Check if private key exists
      if [ ! -f /etc/wireguard/wg0.conf ]; then
        echo "WireGuard configuration not found at /etc/wireguard/wg0.conf"
        echo "Please run the wakasu apply.sh script to set up WireGuard"
        exit 0
      fi

      # Bring up the WireGuard interface
      ${pkgs.wireguard-tools}/bin/wg-quick up wg0 || true
    '';

    preStop = ''
      ${pkgs.wireguard-tools}/bin/wg-quick down wg0 || true
    '';
  };

  # Configure /etc files
  environment.etc = {
    "syncthing-config-notice" = {
      text = ''
        Syncthing is managed by system-manager.
        Configuration is stored in /home/vincent/.config/syncthing

        GUI Address: ${libx.syncthingGuiAddress globals.machines."${hostname}"}

        To manage devices and folders, use the web interface.
      '';
      mode = "0444";
    };
  };
}
