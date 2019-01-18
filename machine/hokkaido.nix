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
  networking = {
    firewall.enable = false; # we are in safe territory :D
    bridges.br1.interfaces = [ "enp0s25" ];
    interfaces.enp0s25 = {
      useDHCP = true;
    };
  };
  profiles = {
    avahi.enable = true;
    dev.enable = true;
    docker.enable = true;
    ssh.enable = true;
    syncthing.enable = true;
    virtualization.enable = true;
    nix-config.buildCores = 2;
  };
  services = {
    logind.extraConfig = "HandleLidSwitch=ignore";    
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
