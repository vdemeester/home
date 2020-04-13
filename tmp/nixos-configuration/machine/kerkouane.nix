{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ../networking.nix ];
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
    wireguard.server.enable = true;
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
      virtualHosts."kerkouane.sbr.pm" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/vincent/desktop/sites/kerkouane.sbr.pm";
        locations."/" = {
          index = "index.html";
        };
      };
      virtualHosts."beta.sbr.pm" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/vincent/desktop/sites/beta.sbr.pm";
        locations."/" = {
          index = "index.html";
        };
      };
      virtualHosts."sbr.pm" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/vincent/desktop/sites/sbr.pm";
        locations."/" = {
          index = "index.html";
        };
      };
      virtualHosts."vincent.demeester.fr" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/vincent/desktop/sites/vincent.demeester.fr";
        locations."/" = {
          index = "index.html";
        };
      };
    };
    openssh.ports = [ ssh.kerkouane.port ];
    openssh.permitRootLogin = "without-password";
    syncthing.guiAddress = "127.0.0.1:8384";
  };
}
