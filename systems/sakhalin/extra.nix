{
  libx,
  globals,
  lib,
  pkgs,
  monitoring,
  config,
  ...
}:
let
  # Get machines that should be monitored
  nodeExporterMachines = monitoring.machinesWithNodeExporter globals.machines;

  # Generate node exporter targets
  nodeExporterTargets = monitoring.mkPrometheusTargets {
    machines = nodeExporterMachines;
    port = 9000;
  };

  # Machines with BIND DNS
  bindMachines = lib.filterAttrs (
    _name: _machine:
    builtins.elem _name [
      "demeter"
      "athena"
    ]
  ) globals.machines;
  bindTargets = monitoring.mkPrometheusTargets {
    machines = bindMachines;
    port = 9009;
  };

  # PostgreSQL hosts
  postgresTargets = map (host: "${host}.sbr.pm:9187") [
    "rhea"
    "sakhalin"
  ];

  # Exportarr services configuration
  exportarrServices = {
    sonarr = {
      port = 9707;
    };
    radarr = {
      port = 9708;
    };
    lidarr = {
      port = 9709;
    };
    prowlarr = {
      port = 9710;
    };
    bazarr = {
      port = 9712;
    };
  };
  exportarrTargets = lib.mapAttrsToList (
    _name: cfg: "rhea.sbr.pm:${toString cfg.port}"
  ) exportarrServices;
in
{

  imports = [
    ../common/services/containers.nix
    ../common/services/docker.nix
    ../common/desktop/binfmt.nix # TODO: move to something else than desktop
    ../common/services/prometheus-exporters-node.nix
    ../common/services/prometheus-exporters-postgres.nix
    ../common/services/linkwarden.nix
  ];

  # Age secrets
  age.secrets."grafana-admin-password" = {
    file = ../../secrets/sakhalin/grafana-admin-password.age;
    mode = "400";
    owner = "grafana";
  };

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
    # PostgreSQL backups
    postgresqlBackup = {
      enable = true;
      databases = [ "linkwarden" ];
      location = "/var/backup/postgresql";
      startAt = "*-*-* 02:15:00"; # Daily at 2:15 AM
    };

    grafana = {
      enable = true;
      settings = {
        server = {
          http_addr = "0.0.0.0";
          http_port = 3000;
          domain = "grafana.sbr.pm";
          root_url = "https://grafana.sbr.pm";
        };
      };

      provision = {
        enable = true;
        datasources.settings = {
          apiVersion = 1;
          datasources = [
            {
              name = "Prometheus";
              type = "prometheus";
              access = "proxy";
              url = "http://localhost:9001";
              isDefault = true;
              jsonData = {
                timeInterval = "30s";
              };
            }
          ];
        };

        dashboards.settings = {
          apiVersion = 1;
          providers = [
            {
              name = "Default";
              type = "file";
              disableDeletion = false;
              allowUiUpdates = true;
              options.path = "/var/lib/grafana/dashboards";
            }
          ];
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
              targets = nodeExporterTargets;
            }
          ];
        }
        {
          job_name = "bind";
          static_configs = [
            {
              targets = bindTargets;
            }
          ];
        }
        {
          job_name = "postgres";
          static_configs = [
            {
              targets = postgresTargets;
            }
          ];
        }
        {
          job_name = "traefik";
          static_configs = [
            {
              targets = [ "rhea.sbr.pm:8080" ];
            }
          ];
        }
        {
          job_name = "caddy";
          static_configs = [
            {
              targets = [ "${builtins.head globals.machines.kerkouane.net.vpn.ips}:2019" ];
            }
          ];
        }
        {
          job_name = "exportarr";
          static_configs = [
            {
              targets = exportarrTargets;
            }
          ];
        }
        {
          job_name = "mosquitto";
          static_configs = [
            {
              targets = [ "demeter.sbr.pm:9234" ];
            }
          ];
        }
        {
          job_name = "homeassistant";
          static_configs = [
            {
              targets = [ "home.sbr.pm:8123" ];
            }
          ];
          metrics_path = "/api/prometheus";
        }
      ];
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

  # Create Grafana dashboard directory
  systemd.tmpfiles.rules = [
    "d /var/lib/grafana/dashboards 0755 grafana grafana -"
  ];

  # Set Grafana admin password from secret file
  systemd.services.grafana-set-admin-password = {
    description = "Set Grafana admin password from secret file";
    after = [ "grafana.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      User = "grafana";
      RemainAfterExit = true;
    };
    script = ''
      # Only set password if admin user exists (database initialized)
      if ${pkgs.grafana}/bin/grafana-cli --homepath /var/lib/grafana admin reset-admin-password --password-from-stdin < ${
        config.age.secrets."grafana-admin-password".path
      } 2>/dev/null; then
        echo "Admin password updated successfully"
      else
        echo "Failed to update password or admin user doesn't exist yet"
      fi
    '';
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
