{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ../hardware/dell-latitude-e6540.nix ./home.nix ];
  networking = {
    firewall.enable = false; # we are in safe territory :D
    bridges.br1.interfaces = [ "eno1" ];
    interfaces.eno1 = {
      useDHCP = true;
    };
  };
  profiles = {
    avahi.enable = true;
    dev.enable = true;
    nix-config.buildCores = 4;
    ssh.enable = true;
    syncthing.enable = true;
    virtualization = {
      enable = true;
      nested = true;
      listenTCP = true;
    };
  };
  services = {
    logind.lidSwitch = "ignore";
    syncthing.guiAddress = "0.0.0.0:8384";
    wireguard = {
      enable = true;
      ips = [ "${wireguard.ips.honshu}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };
  };
}
