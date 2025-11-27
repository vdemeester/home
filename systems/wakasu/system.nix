{
  globals,
  hostname,
  libx,
  pkgs,
  ...
}:
{
  # The syncthing and wireguard modules are imported via mkSystemManager in lib/default.nix

  # Environment packages
  environment.systemPackages = with pkgs; [
    helix
    acpi
  ];

  # Syncthing configuration using the nixpkgs module
  services.syncthing = {
    enable = true;
    user = "vincent";
    group = "users";
    dataDir = "/home/vincent/.local/share/syncthing";
    configDir = "/home/vincent/.config/syncthing";
    guiAddress = libx.syncthingGuiAddress globals.machines."${hostname}";
    openDefaultPorts = false;

    settings = {
      options = {
        urAccepted = -1; # Disable usage reporting
      };
    };
  };

  # WireGuard configuration using the custom wireguard wrapper module
  services.wireguard = {
    enable = true;
    ips = [ "10.100.0.90/24" ];
    allowedIPs = [ "10.100.0.0/24" ];
    endpoint = "167.99.17.238";
    endpointPort = 51820;
    endpointPublicKey = "+H3fxErP9HoFUrPgU19ra9+GDLQw+VwvLWx3lMct7QI=";
  };

  # Configure /etc files
  environment.etc = {
    "syncthing-config-notice" = {
      text = ''
        Syncthing is managed by system-manager using the nixpkgs module.
        Configuration is stored in /home/vincent/.config/syncthing

        GUI Address: ${libx.syncthingGuiAddress globals.machines."${hostname}"}

        To manage devices and folders, use the web interface.
      '';
      mode = "0444";
    };
  };
}
