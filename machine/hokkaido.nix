{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ../hardware/thinkpad-x220.nix ./home.nix ];
  boot = {
    kernel.sysctl = {
      "net.bridge.bridge-nf-call-arptables" = 0;
      "net.bridge.bridge-nf-call-iptables" = 0;
      "net.bridge.bridge-nf-call-ip6tables" = 0;
    };
  };
  hardware.u2f.enable = true;
  profiles = {
    dev.enable = true;
    docker.enable = true;
    laptop.enable = true;
    desktop.autoLogin = true;
    ssh.enable = true;
    syncthing.enable = true;
    nix-config.buildCores = 2;
    yubikey.enable = true;
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
    syncthing-edge.guiAddress = "${wireguard.ips.hokkaido}:8384";
    wireguard = {
      enable = true;
      ips = [ "${wireguard.ips.hokkaido}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };
  };
  environment.systemPackages = with pkgs; [
    nfs-utils
    sshfs
  ];
}
