{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [
    ../networking.nix # generated at runtime by nixos-infect
  ];
  time.timeZone = "Europe/Paris";
  boot = {
    cleanTmpDir = true;
    loader.grub.enable = true;
  };
  profiles = {
    git.enable = true;
    nix-config.localCaches = [];
    nix-config.buildCores = 1;
    ssh.enable = true;
    syncthing.enable = true;
  };
  networking.firewall.allowPing = true;
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  security = {
    acme.certs = {
      "sbr.pm".email = "vincent@sbr.pm";
    };
  };
  services = {
    nginx = {
      enable = true;
      virtualHosts."carthage.sbr.pm" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/vincent/desktop/sites/carthage.sbr.pm";
        locations."/" = {
          index = "index.html";
        };
      };
    };
    openssh.ports = [ ssh.carthage.port ];
    openssh.permitRootLogin = "without-password";
    syncthing.guiAddress = "127.0.0.1:8384";
    wireguard = {
      enable = true;
      ips = [ "${wireguard.ips.carthage}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };
  };
}
