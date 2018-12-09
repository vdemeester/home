{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ../hardware/thinkpad-x220.nix ];
  time.timeZone = "Europe/Paris";
  profiles = {
    avahi.enable = true;
    dev.enable = true;
    ssh.enable = true;
    virtualization.enable = true;
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
