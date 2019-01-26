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
    nix-auto-update.autoUpgrade = false;
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
        root = "/var/www/default";
        locations."/" = {
          index = "index";
        };
      };
      virtualHosts."sbr.pm" = {
        enableACME = true;
        forceSSL = true;
        root = "/var/www/default";
        locations."/" = {
          index = "index";
        };
      };
    };
    openssh.ports = [ ssh.carthage.port ];
    openssh.permitRootLogin = "without-password";
    syncthing-edge.guiAddress = "${wireguard.ips.carthage}:8384";
    wireguard = {
      enable = true;
      ips = [ "${wireguard.ips.carthage}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };
  };
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGR4dqXwHwPpYgyk6yl9+9LRL3qrBZp3ZWdyKaTiXp0p vincent@shikoku"
  ];
}
