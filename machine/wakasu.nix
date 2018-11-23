{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
    ../hardware-configuration.nix
    ../profiles/laptop.nix
    ../profiles/ssh.nix
    ../profiles/dev.nix
    ../profiles/containerd.nix
    ../profiles/dockerization.nix
    ../profiles/virtualization.nix
    ../location/home.nix
    ../hardware/lenovo-p50.nix
  ];

  security.pam.loginLimits = [
    { domain = "@audio"; item = "memlock"; type = "-"; value = "unlimited"; }
    { domain = "@audio"; item = "rtprio";  type = "-"; value = "99"; }
    { domain = "@audio"; item = "nofile";  type = "-"; value = "99999"; }
  ];
  
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  hardware.trackpoint.enable = false;

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
  hardware.nvidia.optimus_prime.enable = true;
  hardware.nvidia.optimus_prime.nvidiaBusId = "PCI:1:0:0";
  hardware.nvidia.optimus_prime.intelBusId = "PCI:0:2:0";

  # Move elsewhere
  programs.podman = {
    enable = true;
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
