{ config, pkgs, ... }:

{
  imports =
    [
      ../location/home.nix
      ../hardware/lenovo-p50.nix
    ];

  profiles.laptop.enable = true;
  profiles.docker.enable = true;
  profiles.containerd.enable = true;
  profiles.virtualization.enable = true;
  profiles.ssh.enable = true;
  profiles.dev.enable = true;

  # Move elsewhere
  programs.podman = {
    enable = true;
  };

  networking.firewall.allowedUDPPortRanges = [ { from = 6001; to = 6101; } ];
  networking.firewall.allowedTCPPorts = [ 7946 9000 5000 ];

  time.timeZone = "Europe/Paris";

  services = {
    logind.extraConfig = ''
      HandleLidSwitch=suspend
      HandleLidSwitchExternalPower=ignore
      HandleLidSwitchDocked=ignore
    '';
    xserver = {
      videoDrivers = [ "nvidia" ];
      dpi = 96;
      displayManager.slim.theme = pkgs.fetchurl {
        url = "https://github.com/vdemeester/slim-themes/raw/master/docker-key-theme-0.1.tar.xz";
        sha256 = "127893l1nzqya0g68k8841g5lm3hlnx7b3b3h06axvplc54a1jd8";
      };
    };
  };

  services.syncthing-edge.guiAddress = with import ../assets/machines.nix; "${wireguard.ips.wakasu}:8384";

  services.wireguard = with import ../assets/machines.nix; {
    enable = true;
    ips = [ "${wireguard.ips.wakasu}/24" ];
    endpoint = wg.endpointIP;
    endpointPort = wg.listenPort;
    endpointPublicKey = wireguard.kerkouane.publicKey;
  };
}
