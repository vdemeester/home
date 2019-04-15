{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ./home.nix ];
  boot = {
    cleanTmpDir = true;
  };
  networking = {
    firewall = {
      allowPing = true;
      allowedTCPPorts = [ 5000 53 80 ];
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
  # -----------------------------------
  environment.etc."vrsync".text = ''
/home/vincent/desktop/pictures/screenshots/ vincent@synodine.local:/volumeUSB2/usbshare/pictures/screenshots/
/home/vincent/desktop/pictures/wallpapers/ vincent@synodine.local:/volumeUSB2/usbshare/pictures/wallpapers/
/home/vincent/desktop/documents/ vincent@synodine.local:/volume1/documents/
/mnt/nyan/photos/ vincent@synodine.local:/volumeUSB2/usbshare/pictures/photos/
/mnt/nyan/music/ vincent@synodine.local:/volumeUSB2/usbshare/music/
  '';
  systemd.services.vrsync = {
    description = "vrsync - sync folders to NAS";
    requires = [ "network-online.target" ];
    after    = [ "network-online.target" ];

    unitConfig.X-StopOnRemoval = false;
    restartIfChanged = false;

    path = with pkgs; [ rsync ];
    script = ''
    ${pkgs.vrsync}/bin/vrsync
    '';

    startAt = "hourly";
    serviceConfig = {
      Type = "oneshot";
      OnFailure = "status-email-root@%n.service";
    };
  };
  # ape – sync git mirrors
  systemd.services.ape = {
    description = "Ape - sync git mirrors";
    requires = [ "network-online.target" ];
    after    = [ "network-online.target" ];

    restartIfChanged = false;
    unitConfig.X-StopOnRemoval = false;

    serviceConfig = {
      Type = "oneshot";
      User = "vincent";
      OnFailure = "status-email-root@%n.service";
    };

    path = with pkgs; [ git ];
    script = ''
    ${pkgs.nur.repos.vdemeester.ape}/bin/ape up /home/vincent/var/mirrors
    '';

    startAt = "hourly";
  };
}
