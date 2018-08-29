{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
    ../hardware-configuration.nix
    ../hardware/dell-latitude-e6540.nix
    ../profiles/server.nix
    ../profiles/dev.nix
    ../profiles/fish.nix
    ../profiles/avahi.nix
    ../profiles/containerd.nix
    ../profiles/syncthing.nix
    ../service/ssh-tunnel.nix
    ../location/home.nix
  ];

  time.timeZone = "Europe/Paris";

  services = {
    logind.extraConfig = "HandleLidSwitch=ignore";
    ssh-tunnel = {
      enable = true;
      localUser = "vincent";
      remoteHostname = "95.85.58.158";
      remotePort = 22;
      remoteUser = "vincent";
      bindPort = 2224;
    };
  };

  networking.enableIPv6 = false;
  networking.firewall.allowedTCPPorts = [ 3389 2375 ];
}
