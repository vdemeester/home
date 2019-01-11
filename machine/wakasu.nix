{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ../hardware/lenovo-p50.nix ./home.nix ];
  profiles = {
    dev.enable = true;
    docker.enable = true;
    laptop.enable = true;
    nix-config.buildCores = 4;
    ssh.enable = true;
    virtualization.enable = true;
  };
  programs = {
    podman.enable = true;
  };
  services = {
    logind.extraConfig = ''
      HandleLidSwitch=suspend
      HandleLidSwitchExternalPower=ignore
      HandleLidSwitchDocked=ignore
    '';
    syncthing-edge.guiAddress = "${wireguard.ips.wakasu}:8384";
    smartd = {
      enable = true;
      devices = [ { device = "/dev/nvme0n1"; } ];
    };
    wireguard = {
      enable = true;
      ips = [ "${wireguard.ips.wakasu}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };
    xserver = {
      videoDrivers = [ "nvidia" ];
      dpi = 96;
    };
  };
}
