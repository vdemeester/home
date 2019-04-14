{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ./home.nix ];
  boot = {
    cleanTmpDir = true;
  };
  networking = {
    firewall = {
      allowPing = true;
      allowedTCPPorts = [ 5000 53 ];
      allowedUDPPorts = [ 53 ];
    };
  };
  profiles = {
    avahi.enable = true;
    git.enable = true;
    nix-config.buildCores = 4;
    ssh.enable = true;
    syncthing.enable = true;
  };
  services = {
    athens = {
      enable = true;
      user = "vincent";
    };
    bind = {
      enable = true;
      forwarders = [ "8.8.8.8" "8.8.4.4" ];
      cacheNetworks = [ "192.168.12.0/24" "127.0.0.0/8" "10.100.0.0/24" ];
      zones = [
        {
          # home
          name = "home";
          slaves = [];
          file = ../assets/db.home;
        }
        {
          # home.reverse
          name = "192.168.12.in-addr.arpa";
          slaves = [];
          file = ../assets/db.192.168.12;
        }
        {
          # vpn
          name = "vpn";
          slaves = [];
          file = ../assets/db.vpn;
        }
        {
          # vpn.reverse
          name = "10.100.0.in-addr.arpa";
          slaves = [];
          file = ../assets/db.10.100.0;
        }
      ];
    };
    dockerRegistry = {
      enable = true;
      enableGarbageCollect = true;
      listenAddress = "0.0.0.0";
      extraConfig = {
        proxy = {
          remoteurl = "https://registry-1.docker.io";
        };
      };
    };
    nix-binary-cache = {
      enable = true;
      domain = "nix.cache.home";
      aliases = ["cache.massimo.home" "nix.okinawa.home"];
    };
    syncthing-edge.guiAddress = "${wireguard.ips.okinawa}:8384";
    tarsnap = {
      enable = true;
      archives = {
        documents = {
          directories = [ "/home/vincent/desktop/documents" ];
          period = "daily";
          keyfile = "/etc/nixos/assets/tarsnap.documents.key";
        };
        org = {
          directories = [ "/home/vincent/desktop/org" ];
          period = "daily";
          keyfile = "/etc/nixos/assets/tarsnap.org.key";
        };
        sites = {
          directories = [ "/home/vincent/desktop/sites" ];
          period = "daily";
          keyfile = "/etc/nixos/assets/tarsnap.sites.key";
        };
      };
    };
    wireguard = {
      enable = true;
      ips = [ "${wireguard.ips.okinawa}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };
  };
}
