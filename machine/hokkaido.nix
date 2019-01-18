{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ../hardware/thinkpad-x220.nix ./home.nix ];
  networking = {
    firewall.allowPing = true;
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
