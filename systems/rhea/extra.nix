{
  libx,
  globals,
  lib,
  pkgs,
  config,
  ...
}:
{
  imports = [
    ../common/services/samba.nix
  ];

  age.secrets."gandi.env" = {
    file = ../../secrets/rhea/gandi.env.age;
    mode = "400";
    owner = "traefik";
    group = "traefik";
  };

  age.secrets."exportarr-sonarr-apikey" = {
    file = ../../secrets/rhea/exportarr-sonarr-apikey.age;
  };
  age.secrets."exportarr-radarr-apikey" = {
    file = ../../secrets/rhea/exportarr-radarr-apikey.age;
  };
  age.secrets."exportarr-lidarr-apikey" = {
    file = ../../secrets/rhea/exportarr-lidarr-apikey.age;
  };
  age.secrets."exportarr-prowlarr-apikey" = {
    file = ../../secrets/rhea/exportarr-prowlarr-apikey.age;
  };
  age.secrets."exportarr-readarr-apikey" = {
    file = ../../secrets/rhea/exportarr-readarr-apikey.age;
  };
  age.secrets."exportarr-bazarr-apikey" = {
    file = ../../secrets/rhea/exportarr-bazarr-apikey.age;
  };

  users.users.vincent.linger = true;

  services = {
    traefik = {
      enable = true;

      staticConfigOptions = {
        # Entry points
        entryPoints = {
          web = {
            address = ":80";
            http.redirections.entryPoint = {
              to = "websecure";
              scheme = "https";
            };
          };
          websecure = {
            address = ":443";
          };
          mqtt = {
            address = ":1883";
          };
          mqtts = {
            address = ":8883";
          };
        };

        # Certificate resolver using Gandi DNS
        certificatesResolvers.letsencrypt = {
          acme = {
            email = "vincent@sbr.pm";
            storage = "/var/lib/traefik/acme.json";
            dnsChallenge = {
              provider = "gandiv5";
              delayBeforeCheck = "0s";
              resolvers = [
                "1.1.1.1:53"
                "8.8.8.8:53"
              ];
            };
          };
        };
      };

      # Dynamic configuration using module option
      dynamicConfigOptions =
        let
          # Helper function to create a simple HTTP router
          mkRouter = name: hosts: {
            rule = lib.concatStringsSep " || " (map (host: "Host(`${host}`)") hosts);
            service = name;
            entryPoints = [ "websecure" ];
            tls.certResolver = "letsencrypt";
          };

          # Helper function to create a simple HTTP service
          mkService = url: {
            loadBalancer.servers = [ { inherit url; } ];
          };

          # Define local services with their ports and optional alternate hosts
          localServices = {
            jellyfin.port = 8096;
            jellyseerr.port = 5055;
            sonarr.port = 8989;
            radarr.port = 7878;
            lidarr.port = 8686;
            bazarr.port = 6767;
            transmission = {
              port = 9091;
              altHosts = [ "t.sbr.pm" ];
            };
            immich.port = 2283;
          };

          # Generate routers for local services
          localRouters = lib.mapAttrs' (
            name: cfg:
            let
              hosts = [ "${name}.sbr.pm" ] ++ (cfg.altHosts or [ ]);
            in
            lib.nameValuePair name (mkRouter name hosts)
          ) localServices;

          # Generate services for local services
          localHttpServices = lib.mapAttrs' (
            name: cfg: lib.nameValuePair name (mkService "http://localhost:${toString cfg.port}")
          ) localServices;

          # Filter machines that have syncthing configured
          syncthingMachines = lib.filterAttrs (
            _name: machine: machine ? syncthing && machine.syncthing ? folders
          ) globals.machines;

          # Generate routers for syncthing hosts
          syncthingRouters = lib.mapAttrs' (
            name: _machine:
            lib.nameValuePair "syncthing-${name}" {
              rule = "Host(`syncthing.sbr.pm`) && PathPrefix(`/${name}`) || Host(`s.sbr.pm`) && PathPrefix(`/${name}`)";
              service = "syncthing-${name}";
              entryPoints = [ "websecure" ];
              middlewares = [
                "syncthing-${name}-addslash"
                "syncthing-${name}-strip"
              ];
              tls = {
                certResolver = "letsencrypt";
              };
            }
          ) syncthingMachines;

          # Generate services for syncthing hosts
          syncthingServices = lib.mapAttrs' (
            name: machine:
            lib.nameValuePair "syncthing-${name}" {
              loadBalancer = {
                servers = [
                  { url = "http://${builtins.head machine.net.vpn.ips}:8384"; }
                ];
              };
            }
          ) syncthingMachines;

          # Generate middleware for path stripping
          syncthingMiddlewares = lib.mapAttrs' (
            name: _machine:
            lib.nameValuePair "syncthing-${name}-strip" {
              stripPrefix = {
                prefixes = [ "/${name}" ];
              };
            }
          ) syncthingMachines;

          # Generate middleware for adding trailing slash
          syncthingAddSlashMiddlewares = lib.mapAttrs' (
            name: _machine:
            lib.nameValuePair "syncthing-${name}-addslash" {
              redirectRegex = {
                regex = "^(https?://[^/]+/${name})$";
                replacement = "$${1}/";
                permanent = true;
              };
            }
          ) syncthingMachines;
        in
        {
          http = {
            routers =
              syncthingRouters
              // localRouters
              // {
                kiwix = mkRouter "kiwix" [ "kiwix.sbr.pm" ];
                n8n = mkRouter "n8n" [ "n8n.sbr.pm" ];
                paperless = mkRouter "paperless" [ "paperless.sbr.pm" ];
                grafana = mkRouter "grafana" [ "grafana.sbr.pm" ];
              };
            services =
              syncthingServices
              // localHttpServices
              // {
                kiwix = mkService "http://${builtins.head globals.machines.sakhalin.net.ips}:8080";
                n8n = mkService "http://${builtins.head globals.machines.sakhalin.net.ips}:5678";
                paperless = mkService "http://${builtins.head globals.machines.sakhalin.net.ips}:8000";
                grafana = mkService "http://${builtins.head globals.machines.sakhalin.net.ips}:3000";
              };
            middlewares = syncthingMiddlewares // syncthingAddSlashMiddlewares;
          };
          tcp = {
            routers = {
              mqtt = {
                rule = "HostSNI(`*`)";
                service = "mqtt";
                entryPoints = [ "mqtt" ];
              };
              mqtts = {
                rule = "HostSNI(`mqtt.sbr.pm`)";
                service = "mqtt";
                entryPoints = [ "mqtts" ];
                tls = {
                  certResolver = "letsencrypt";
                };
              };
            };
            services = {
              mqtt = {
                loadBalancer = {
                  servers = [
                    { address = "${builtins.head globals.machines.demeter.net.vpn.ips}:1883"; }
                  ];
                };
              };
            };
          };
        };
    };

    wireguard = {
      enable = true;
      ips = libx.wg-ips globals.machines.rhea.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
    # smartd = {
    #   enable = true;
    #   devices = [ { device = "/dev/nvme0n1"; } ];
    # };
    samba.settings = {
      global."server string" = "Rhea";
      backup = {
        path = "/neo/backup";
        public = "yes";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        writable = "yes";
        comment = "backup";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "vincent";
        "force group" = "users";
      };
      documents = {
        path = "/neo/documents";
        public = "yes";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        writable = "yes";
        comment = "documents";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "vincent";
        "force group" = "users";
      };
      downloads = {
        path = "/neo/downloads";
        public = "yes";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        writable = "yes";
        comment = "downloads";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "vincent";
        "force group" = "users";
      };
      music = {
        path = "/neo/music";
        public = "yes";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        writable = "yes";
        comment = "music";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "vincent";
        "force group" = "users";
      };
      pictures = {
        path = "/neo/pictures";
        public = "yes";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        writable = "yes";
        comment = "pictures";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "vincent";
        "force group" = "users";
      };
      videos = {
        path = "/neo/videos";
        public = "yes";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        writable = "yes";
        comment = "videos";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "vincent";
        "force group" = "users";
      };
    };
    nfs.server = {
      enable = true;
      exports = ''
                /neo                      192.168.1.0/24(rw,fsid=0,no_subtree_check) 10.100.0.0/24(rw,fsid=0,no_subtree_check)
                /neo/backup               192.168.1.0/24(rw,fsid=1,no_subtree_check) 10.100.0.0/24(rw,fsid=1,no_subtree_check)
                /neo/documents            192.168.1.0/24(rw,fsid=2,no_subtree_check) 10.100.0.0/24(rw,fsid=2,no_subtree_check)
                /neo/downloads            192.168.1.0/24(rw,fsid=3,no_subtree_check) 10.100.0.0/24(rw,fsid=3,no_subtree_check)
                /neo/music                192.168.1.0/24(rw,fsid=4,no_subtree_check) 10.100.0.0/24(rw,fsid=4,no_subtree_check)
                /neo/pictures             192.168.1.0/24(rw,fsid=5,no_subtree_check) 10.100.0.0/24(rw,fsid=5,no_subtree_check)
                /neo/videos               192.168.1.0/24(rw,fsid=6,no_subtree_check) 10.100.0.0/24(rw,fsid=6,no_subtree_check)
        			'';
    };
    immich = {
      enable = true;
      user = "vincent";
      group = "users";
      mediaLocation = "/neo/pictures/photos";
    };
    postgresql = {
      ensureDatabases = [ "immich" ];
      ensureUsers = [
        {
          name = "vincent";
        }
      ];
    };
    jellyfin = {
      enable = true;
      user = "vincent";
      group = "users";
      openFirewall = true;
    };
    jellyseerr = {
      enable = true;
      openFirewall = true;
    };
    aria2 = {
      # FIXME: make sure aria2 runs as user vincent
      enable = true;
      openPorts = true;
      settings = {
        max-concurrent-downloads = 20;
        dir = "/neo/downloads";
      };
      rpcSecretFile = "${pkgs.writeText "aria" "aria2rpc\n"}"; # FIXME: use secrets for this somehow
    };
    transmission = {
      enable = true;
      user = "vincent";
      group = "users";
      openFirewall = true;
      package = pkgs.transmission_4;
      openRPCPort = true; # Open firewall for RPC
      home = "/neo/torrents";
      settings = {
        # Override default settings
        incomplete-dir-enabled = true;
        rpc-bind-address = "0.0.0.0"; # Bind to own IP
        rpc-host-whitelist = "localhost,t.sbr.pm,transmission.sbr.pm,rhea.home,rhea.vpn,rhea.sbr.pm,192.168.1.50,10.100.0.50";
        rpc-host-whitelist-enabled = true;
        rpc-whitelist-enabled = true;
        rpc-whitelist = "127.0.0.1,192.168.1.*,10.100.0.*"; # Whitelist your remote machine (10.0.0.1 in this example)
        rpc-username = "transmission";
        rpc-password = "transmission";
        download-queue-enabled = true;
        download-queue-size = 15;
        queue-stalled-enabled = true;
        queue-stalled-minutes = 30;
        ratio-limit = 0.1;
        ratio-limit-enabled = true;
      };
    };
    sonarr = {
      enable = true;
      user = "vincent";
      group = "users";
      openFirewall = true;
    };
    radarr = {
      enable = true;
      user = "vincent";
      group = "users";
      openFirewall = true;
    };
    bazarr = {
      enable = true;
      user = "vincent";
      group = "users";
      openFirewall = true;
    };
    prowlarr = {
      enable = true;
      openFirewall = true;
    };
    readarr = {
      enable = true;
      user = "vincent";
      group = "users";
      openFirewall = true;
    };
    lidarr = {
      enable = true;
      user = "vincent";
      group = "users";
      openFirewall = true;
    };
    prometheus.exporters = {
      exportarr-sonarr = {
        enable = true;
        port = 9707;
        url = "http://localhost:8989";
        apiKeyFile = config.age.secrets."exportarr-sonarr-apikey".path;
      };
      exportarr-radarr = {
        enable = true;
        port = 9708;
        url = "http://localhost:7878";
        apiKeyFile = config.age.secrets."exportarr-radarr-apikey".path;
      };
      exportarr-lidarr = {
        enable = true;
        port = 9709;
        url = "http://localhost:8686";
        apiKeyFile = config.age.secrets."exportarr-lidarr-apikey".path;
      };
      exportarr-prowlarr = {
        enable = true;
        port = 9710;
        url = "http://localhost:9696";
        apiKeyFile = config.age.secrets."exportarr-prowlarr-apikey".path;
      };
      exportarr-readarr = {
        enable = true;
        port = 9711;
        url = "http://localhost:8787";
        apiKeyFile = config.age.secrets."exportarr-readarr-apikey".path;
      };
      exportarr-bazarr = {
        enable = true;
        port = 9712;
        url = "http://localhost:6767";
        apiKeyFile = config.age.secrets."exportarr-bazarr-apikey".path;
      };
    };
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "vincent@sbr.pm";
  };

  # Grant vincent ownership of the immich database and schemas
  systemd.services.postgresql.postStart = lib.mkAfter ''
    $PSQL -tAc "SELECT 1 FROM pg_roles WHERE rolname = 'vincent'" | grep -q 1 || $PSQL -tAc "CREATE ROLE vincent WITH LOGIN"
    $PSQL -tAc "ALTER DATABASE immich OWNER TO vincent"
    $PSQL immich -tAc "ALTER SCHEMA public OWNER TO vincent"
    $PSQL immich -tAc "ALTER SCHEMA vectors OWNER TO vincent" || true
    $PSQL immich -tAc "GRANT ALL PRIVILEGES ON SCHEMA public TO vincent"
    $PSQL immich -tAc "GRANT ALL PRIVILEGES ON SCHEMA vectors TO vincent" || true
    $PSQL immich -tAc "GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO vincent"
    $PSQL immich -tAc "GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO vincent"
    $PSQL immich -tAc "GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA vectors TO vincent" || true
    $PSQL immich -tAc "ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TABLES TO vincent"
    $PSQL immich -tAc "ALTER DEFAULT PRIVILEGES IN SCHEMA vectors GRANT ALL ON TABLES TO vincent" || true
  '';

  networking.useDHCP = lib.mkDefault true;

  # Open firewall for Traefik
  networking.firewall.allowedTCPPorts = [
    80
    443
    1883 # MQTT
    8883 # MQTTS
  ];

  # Environment file for Gandi API key (managed by agenix)
  systemd.services.traefik.serviceConfig = {
    EnvironmentFile = config.age.secrets."gandi.env".path;
  };

  environment.systemPackages = with pkgs; [
    lm_sensors
    gnumake
  ];

}
