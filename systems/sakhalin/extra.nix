{
  libx,
  globals,
  lib,
  pkgs,
  ...
}:
{

  imports = [
    ../common/services/containers.nix
    ../common/services/docker.nix
    ../common/desktop/binfmt.nix # TODO: move to something else than desktop
    ../common/services/prometheus-exporters-node.nix
  ];

  # TODO make it an option ? (otherwise I'll add it for all)
  users.users.vincent.linger = true;

  systemd.services.n8n.environment = {
    N8N_SECURE_COOKIE = "false";
    PATH = lib.mkForce "/run/current-system/sw/bin";
  };

  services = {
    atuin = {
      enable = true;
      host = "0.0.0.0";
      openRegistration = false;
    };

    n8n = {
      enable = true;
      openFirewall = true;
      # webhookUrl = "";
    };
    paperless = {
      enable = true;
      address = "${builtins.head globals.machines.sakhalin.net.vpn.ips}";
    };
    # services.postgresql.enable = true;
    # services.postgresql.package = pkgs.postgresql_15;
    # services.postgresql.dataDir = "/var/lib/postgresql/15";
    # services.postgresqlBackup.databases = [ "atuin" "homepage_production" "nextcloud" ];
    # services.postgresqlBackup.enable = true;
    # services.postgresqlBackup.location = "/var/backup/postgresql";
    # services.postgresqlBackup.startAt = "*-*-* 02:15:00";

    grafana = {
      enable = true;
      settings = {
        server = {
          http_addr = "0.0.0.0";
          http_port = 3000;
          domain = "graphana.sbr.pm";
        };
      };
    };
    prometheus = {
      enable = true;
      port = 9001;
      scrapeConfigs = [
        {
          job_name = "node";
          static_configs = [
            {
              # TODO: make this dynamic
              targets = [
                "aion.sbr.pm:9100"
                "aix.sbr.pm:9000"
                "aomi.sbr.pm:9000"
                "athena.sbr.pm:9000"
                "demeter.sbr.pm:9000"
                "kerkouane.sbr.pm:9000"
                "sakhalin.sbr.pm:9000"
                "shikoku.sbr.pm:9000"
              ];
            }
          ];
        }
        {
          job_name = "bind";
          static_configs = [
            {
              targets = [
                "demeter.sbr.pm:9009"
                "athena.sbr.pm:9009"
              ];
            }
          ];
        }
        {
          job_name = "nginx";
          static_configs = [
            {
              targets = [ "kerkouane.sbr.pm:9001" ];
            }
          ];
        }
        {
          job_name = "exportarr";
          static_configs = [
            {
              targets = [
                "rhea.sbr.pm:9707" # sonarr
                "rhea.sbr.pm:9708" # radarr
                "rhea.sbr.pm:9709" # lidarr
                "rhea.sbr.pm:9710" # prowlarr
                "rhea.sbr.pm:9711" # readarr
                "rhea.sbr.pm:9712" # bazarr
              ];
            }
          ];
        }
      ];
      exporters.node = {
        enable = true;
        port = 9000;
        enabledCollectors = [
          "systemd"
          "processes"
        ];
        extraFlags = [
          "--collector.ethtool"
          "--collector.softirqs"
          "--collector.tcpstat"
        ];
      };
    };
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
      };
    };
    nfs.server = {
      enable = true;
      exports = ''
        /export                      192.168.1.0/24(rw,fsid=0,no_subtree_check) 10.100.0.0/24(rw,fsid=0,no_subtree_check)
        /export/gaia                 192.168.1.0/24(rw,fsid=1,no_subtree_check) 10.100.0.0/24(rw,fsid=1,no_subtree_check)
        /export/toshito              192.168.1.0/24(rw,fsid=2,no_subtree_check) 10.100.0.0/24(rw,fsid=2,no_subtree_check)
      '';
    };

    wireguard = {
      enable = true;
      ips = libx.wg-ips globals.machines.sakhalin.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
  };
  environment.systemPackages = with pkgs; [ yt-dlp ]; # -----------------------------------
  environment.etc."vrsync".text = ''
    /home/vincent/desktop/pictures/screenshots/ vincent@synodine.home:/volumeUSB2/usbshare/pictures/screenshots/
    /home/vincent/desktop/pictures/wallpapers/ vincent@synodine.home:/volumeUSB2/usbshare/pictures/wallpapers/
    /home/vincent/desktop/documents/ vincent@synodine.home:/volume1/documents/
    /mnt/gaia/photos/ vincent@synodine.home:/volumeUSB2/usbshare/pictures/photos/
    /mnt/gaia/music/ vincent@synodine.home:/volumeUSB2/usbshare/music/
  '';
  systemd.services.vrsync = {
    description = "vrsync - sync folders to NAS";
    requires = [ "network-online.target" ];
    after = [ "network-online.target" ];

    unitConfig.X-StopOnRemoval = false;
    restartIfChanged = false;

    path = with pkgs; [
      rsync
      coreutils
      bash
      openssh
    ];
    script = ''
      ${pkgs.vrsync}/bin/vrsync
    '';

    startAt = "hourly";
    serviceConfig = {
      Type = "oneshot";
      OnFailure = "status-email-root@%n.service";
    };
  };
  # mr -i u daily
  systemd.services.mr = {
    description = "Update configs daily";
    requires = [ "network-online.target" ];
    after = [ "network-online.target" ];

    restartIfChanged = false;
    unitConfig.X-StopOnRemoval = false;

    serviceConfig = {
      Type = "oneshot";
      User = "vincent";
      OnFailure = "status-email-root@%n.service";
    };

    path = with pkgs; [
      git
      mr
    ];
    script = ''
      set -e
       cd /mnt/gaia/src/configs/
       mr -t run git reset --hard
       mr -t u
    '';

    startAt = "daily";
  };
  # Kiwix serve
  systemd.services.kiwix-serve = {
    description = "Kiwix offline content server";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];

    serviceConfig = {
      Type = "simple";
      User = "vincent";
      ExecStart = "${pkgs.bash}/bin/bash -c '${pkgs.kiwix-tools}/bin/kiwix-serve --port=8080 /mnt/gaia/kiwix/*.zim'";
      Restart = "on-failure";
      RestartSec = "5s";
    };
  };
}
