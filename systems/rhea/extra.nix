{
  libx,
  globals,
  lib,
  pkgs,
  config,
  ...
}:

let
  # Service defaults for media/homelab services
  serviceDefaults = libx.mkServiceDefaults { };

  # Samba shares configuration (data-driven approach)
  sambaShares = [
    "audiobooks"
    "ebooks"
    "backup"
    "documents"
    "downloads"
    "music"
    "pictures"
    "videos"
  ];

  # Exportarr services configuration (data-driven approach)
  exportarrServices = {
    sonarr = {
      port = 9707;
      servicePort = 8989;
    };
    radarr = {
      port = 9708;
      servicePort = 7878;
    };
    lidarr = {
      port = 9709;
      servicePort = 8686;
    };
    prowlarr = {
      port = 9710;
      servicePort = 9696;
    };
    bazarr = {
      port = 9712;
      servicePort = 6767;
    };
  };

  # Common rsync configuration for aion backups (reverse sync after migration)
  aionBackupDefaults = {
    source = {
      host = "aion.sbr.pm";
      user = "vincent";
    };
    destination = "/neo";
    delete = true; # Mirror mode: delete files in destination that don't exist in source
    user = "vincent";
    group = "users";
    rsyncArgs = [
      "--exclude=.Trash-*"
      "--exclude=lost+found"
    ];
    sshArgs = [
      "-o StrictHostKeyChecking=accept-new"
    ];
  };
in
{
  imports = [
    ../common/services/samba.nix
    ../common/services/prometheus-exporters-node.nix
    ../common/services/prometheus-exporters-postgres.nix
    ../../modules/jellyfin-auto-collections
  ];

  # Age secrets: gandi.env + webdav + jellyfin + generated exportarr secrets
  age.secrets = {
    "gandi.env" = {
      file = ../../secrets/rhea/gandi.env.age;
      mode = "400";
      owner = "traefik";
      group = "traefik";
    };
    "webdav-password" = {
      file = ../../secrets/rhea/webdav-password.age;
      mode = "400";
    };
    "jellyfin-auto-collections-api-key" = {
      file = ../../secrets/rhea/jellyfin-auto-collections-api-key.age;
      mode = "400";
      owner = "jellyfin-auto-collections";
    };
    "jellyfin-auto-collections-jellyseerr-password" = {
      file = ../../secrets/rhea/jellyfin-auto-collections-jellyseerr-password.age;
      mode = "400";
      owner = "jellyfin-auto-collections";
    };
  }
  // lib.mapAttrs' (
    name: _cfg:
    lib.nameValuePair "exportarr-${name}-apikey" {
      file = ../../secrets/rhea/exportarr-${name}-apikey.age;
      mode = "440";
      group = "homepage";
    }
  ) exportarrServices;

  users.users.vincent.linger = true;

  services = {
    traefik = {
      enable = true;

      staticConfigOptions = {
        # API and Dashboard
        api = {
          dashboard = true;
          insecure = false;
        };

        # Prometheus metrics
        metrics.prometheus = {
          addEntryPointsLabels = true;
          addRoutersLabels = true;
          addServicesLabels = true;
        };

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
            transport = {
              respondingTimeouts = {
                readTimeout = "600s"; # 10 minutes for large uploads
                writeTimeout = "600s";
                idleTimeout = "600s";
              };
            };
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

          # Helper function to create a router with middlewares
          mkRouterWithMiddlewares = name: hosts: middlewares: {
            rule = lib.concatStringsSep " || " (map (host: "Host(`${host}`)") hosts);
            service = name;
            entryPoints = [ "websecure" ];
            tls.certResolver = "letsencrypt";
            inherit middlewares;
          };

          # Helper function to create a simple HTTP service
          mkService = url: {
            loadBalancer.servers = [ { inherit url; } ];
          };

          # Define local services with their ports and optional alternate hosts
          localServices = {
            jellyfin.port = 8096;
            jellyseerr.port = 5055;
            # *arr services - ports from exportarrServices
            sonarr.port = exportarrServices.sonarr.servicePort;
            radarr.port = exportarrServices.radarr.servicePort;
            lidarr.port = exportarrServices.lidarr.servicePort;
            bazarr.port = exportarrServices.bazarr.servicePort;
            prowlarr.port = exportarrServices.prowlarr.servicePort;
            transmission = {
              port = 9091;
              altHosts = [ "t.sbr.pm" ];
            };
            immich.port = 2283;
            audiobookshelf = {
              port = 13378;
              altHosts = [ "podcasts.sbr.pm" ];
            };
            calibre = {
              port = 8083;
              altHosts = [ "books.sbr.pm" ];
            };
            dav.port = 6065;
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
                replacement = "$$1/";
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
                # Override immich router to add large file upload middleware
                immich = mkRouterWithMiddlewares "immich" [ "immich.sbr.pm" ] [ "immich-buffering" ];
                # Override home router to add Home Assistant headers
                home = mkRouterWithMiddlewares "home" [ "home.sbr.pm" ] [ "home-headers" ];
                kiwix = mkRouter "kiwix" [ "kiwix.sbr.pm" ];
                n8n = mkRouter "n8n" [ "n8n.sbr.pm" ];
                paperless = mkRouter "paperless" [ "paperless.sbr.pm" ];
                grafana = mkRouter "grafana" [ "grafana.sbr.pm" ];
                navidrome = mkRouter "navidrome" [
                  "navidrome.sbr.pm"
                  "music.sbr.pm"
                ];
                transmission-music = mkRouter "transmission-music" [
                  "transmission-music.sbr.pm"
                  "tm.sbr.pm"
                ];
                linkwarden = mkRouter "linkwarden" [
                  "linkwarden.sbr.pm"
                  "links.sbr.pm"
                ];
                homepage = mkRouter "homepage" [ "homepage.sbr.pm" ];
                # Traefik dashboard
                traefik-dashboard = {
                  rule = "Host(`traefik.sbr.pm`)";
                  service = "api@internal";
                  entryPoints = [ "websecure" ];
                  tls.certResolver = "letsencrypt";
                };
              };
            services =
              syncthingServices
              // localHttpServices
              // {
                home = mkService "http://${builtins.head globals.machines.hass.net.ips}:8123";
                kiwix = mkService "http://${builtins.head globals.machines.sakhalin.net.ips}:8080";
                n8n = mkService "http://${builtins.head globals.machines.sakhalin.net.ips}:5678";
                paperless = mkService "http://${builtins.head globals.machines.sakhalin.net.ips}:8000";
                grafana = mkService "http://${builtins.head globals.machines.sakhalin.net.ips}:3000";
                linkwarden = mkService "http://${builtins.head globals.machines.sakhalin.net.ips}:3002";
                navidrome = mkService "http://${builtins.head globals.machines.aion.net.ips}:4533";
                transmission-music = mkService "http://${builtins.head globals.machines.aion.net.ips}:9091";
                homepage = mkService "http://${builtins.head globals.machines.aion.net.ips}:3001";
              };
            middlewares =
              syncthingMiddlewares
              // syncthingAddSlashMiddlewares
              // {
                # Middleware for handling large file uploads (Immich)
                immich-buffering = {
                  buffering = {
                    maxRequestBodyBytes = 0; # No limit
                    memRequestBodyBytes = 104857600; # 100MB in memory
                    maxResponseBodyBytes = 0; # No limit
                    memResponseBodyBytes = 104857600; # 100MB in memory
                    retryExpression = "IsNetworkError() && Attempts() < 2";
                  };
                };
                # Middleware for Home Assistant reverse proxy headers
                home-headers = {
                  headers = {
                    customRequestHeaders = {
                      X-Forwarded-Proto = "https";
                    };
                  };
                };
              };
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
                    { address = "${builtins.head globals.machines.demeter.net.ips}:1883"; }
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
    }
    // builtins.listToAttrs (
      map (
        name:
        lib.nameValuePair name (
          libx.mkSambaShare {
            inherit name;
            path = "/neo/${name}";
          }
        )
      ) sambaShares
    );
    nfs.server = {
      enable = true;
      # Fixed ports for firewall configuration
      lockdPort = 4001;
      mountdPort = 4002;
      statdPort = 4000;
      exports = ''
                /neo                      192.168.1.0/24(rw,fsid=0,no_subtree_check) 10.100.0.0/24(rw,fsid=0,no_subtree_check)
                /neo/audiobooks           192.168.1.0/24(rw,fsid=1,no_subtree_check) 10.100.0.0/24(rw,fsid=1,no_subtree_check)
                /neo/backup               192.168.1.0/24(rw,fsid=2,no_subtree_check) 10.100.0.0/24(rw,fsid=2,no_subtree_check)
                /neo/documents            192.168.1.0/24(rw,fsid=3,no_subtree_check) 10.100.0.0/24(rw,fsid=3,no_subtree_check)
                /neo/downloads            192.168.1.0/24(rw,fsid=4,no_subtree_check) 10.100.0.0/24(rw,fsid=4,no_subtree_check)
                /neo/ebooks               192.168.1.0/24(rw,fsid=5,no_subtree_check) 10.100.0.0/24(rw,fsid=5,no_subtree_check)
                /neo/music                192.168.1.0/24(rw,fsid=6,no_subtree_check) 10.100.0.0/24(rw,fsid=6,no_subtree_check)
                /neo/pictures             192.168.1.0/24(rw,fsid=7,no_subtree_check) 10.100.0.0/24(rw,fsid=7,no_subtree_check)
                /neo/videos               192.168.1.0/24(rw,fsid=8,no_subtree_check) 10.100.0.0/24(rw,fsid=8,no_subtree_check)
        			'';
    };
    immich = serviceDefaults // {
      enable = true;
      mediaLocation = "/neo/pictures/photos";
    };
    postgresql = {
      ensureDatabases = [
        "immich"
      ];
      ensureUsers = [
        {
          name = "vincent";
        }
      ];
    };
    jellyfin = serviceDefaults // {
      enable = true;
    };
    jellyseerr = {
      enable = true;
      openFirewall = true;
    };
    webdav = {
      enable = true;
      user = "vincent";
      group = "users";
      environmentFile = config.age.secrets."webdav-password".path;
      settings = {
        address = "127.0.0.1";
        port = 6065;
        scope = "/neo/documents/boox";
        modify = true;
        users = [
          {
            username = "vincent";
            password = "{env}WEBDAV_PASSWORD_HASH";
          }
        ];
        rules = [
          {
            regex = "(\\..*|.*\\.tmp)$"; # Block hidden files and .tmp files
            allow = false;
          }
        ];
      };
    };
    jellyfin-auto-collections = {
      enable = true;
      jellyfinUrl = "http://localhost:8096";
      userId = "400fef4e0ab2448cb8a2bc8ca2facc4f";
      apiKeyFile = config.age.secrets."jellyfin-auto-collections-api-key".path;
      schedule = "daily"; # Run daily at midnight

      jellyseerr = {
        enable = false; # Enable when password secret is created
        serverUrl = "http://localhost:5055";
        email = "vincent@sbr.pm";
        # Uncomment when jellyseerr password secret is created
        # passwordFile = config.age.secrets."jellyfin-auto-collections-jellyseerr-password".path;
        userType = "local";
      };

      settings = {
        plugins = {
          imdb_chart = {
            enabled = true;
            list_ids = [
              "top"
              "moviemeter"
            ];
            clear_collection = true;
          };
          imdb_list = {
            enabled = true;
            list_ids = [
              "ls055592025" # IMDb Top 250
            ];
          };
          jellyfin_api = {
            enabled = true;
            list_ids = [
              # Marvel Cinematic Universe
              {
                studios = [
                  "Marvel Studios"
                  "Marvel Entertainment"
                ];
                list_name = "Marvel Cinematic Universe";
                includeItemTypes = [ "Movie" ];
              }
              # Pixar Animation
              {
                studios = [ "Pixar" ];
                list_name = "Pixar Collection";
                includeItemTypes = [ "Movie" ];
              }
              # Studio Ghibli
              {
                studios = [ "Studio Ghibli" ];
                list_name = "Studio Ghibli Collection";
                includeItemTypes = [ "Movie" ];
              }
              # Sing Movies (Illumination)
              {
                searchTerm = "Sing";
                studios = [ "Illumination Entertainment" ];
                list_name = "Sing Movies";
                includeItemTypes = [ "Movie" ];
              }
              # Christopher Nolan Films
              {
                person = [ "Christopher Nolan" ];
                list_name = "Christopher Nolan Collection";
                includeItemTypes = [ "Movie" ];
              }
              # Highly Rated Sci-Fi
              {
                genres = [ "Science Fiction" ];
                minCriticRating = [ "8" ];
                list_name = "Top Sci-Fi Movies";
                includeItemTypes = [ "Movie" ];
              }
              # Recent Movies (2024-2025)
              {
                years = [
                  2024
                  2025
                ];
                list_name = "Recent Releases";
                includeItemTypes = [ "Movie" ];
              }
              # Award Winners
              {
                tags = [ "Oscar Winner" ];
                list_name = "Oscar Winners";
                includeItemTypes = [ "Movie" ];
              }
            ];
          };
        };
      };
    };
    transmission = serviceDefaults // {
      enable = true;
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
    # *arr services - ports configured via exportarrServices
    sonarr = serviceDefaults // {
      enable = true;
      settings.server.port = exportarrServices.sonarr.servicePort;
    };
    radarr = serviceDefaults // {
      enable = true;
      settings.server.port = exportarrServices.radarr.servicePort;
    };
    bazarr = serviceDefaults // {
      enable = true;
      listenPort = exportarrServices.bazarr.servicePort;
    };
    prowlarr = {
      enable = true;
      openFirewall = true;
      settings.server.port = exportarrServices.prowlarr.servicePort;
    };

    # Rsync replica jobs to backup FROM aion (disabled until migration)
    rsync-replica = {
      enable = true; # Enable after audio services migration to aion
      jobs = {
        aion-music-hourly = aionBackupDefaults // {
          source = aionBackupDefaults.source // {
            paths = [ "/neo/music" ];
          };
          schedule = "hourly";
        };
        aion-audiobooks-daily = aionBackupDefaults // {
          source = aionBackupDefaults.source // {
            paths = [ "/neo/audiobooks" ];
          };
          schedule = "daily";
        };
      };
    };

    # Generate prometheus exporters for all exportarr services
    prometheus.exporters = lib.mapAttrs' (
      name: cfg:
      lib.nameValuePair "exportarr-${name}" {
        enable = true;
        port = cfg.port;
        url = "http://localhost:${toString cfg.servicePort}";
        apiKeyFile = config.age.secrets."exportarr-${name}-apikey".path;
      }
    ) exportarrServices;
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "vincent@sbr.pm";
  };

  # Grant vincent ownership and superuser privileges for the immich database
  # Grant healthchecks user permissions for the healthchecks database
  systemd.services.postgresql.postStart = lib.mkAfter ''
    PSQL="${config.services.postgresql.package}/bin/psql --port=${toString config.services.postgresql.settings.port}"
    $PSQL -tAc "SELECT 1 FROM pg_roles WHERE rolname = 'vincent'" | grep -q 1 || $PSQL -tAc "CREATE ROLE vincent WITH LOGIN SUPERUSER"
    $PSQL -tAc "ALTER ROLE vincent WITH SUPERUSER"
    $PSQL -tAc "ALTER DATABASE immich OWNER TO vincent"
    $PSQL immich -tAc "ALTER SCHEMA public OWNER TO vincent"
    $PSQL immich -tAc "GRANT ALL PRIVILEGES ON SCHEMA public TO vincent"
    $PSQL immich -tAc "GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO vincent"
    $PSQL immich -tAc "GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO vincent"
    $PSQL immich -tAc "ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT ALL ON TABLES TO vincent"
  '';

  # Calibre Content Server for ebook library
  systemd.services.calibre-server = {
    description = "Calibre Content Server";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.calibre}/bin/calibre-server --port=8083 /neo/ebooks";
      Restart = "on-failure";
      User = "vincent";
      Group = "users";
    };
  };

  networking.useDHCP = lib.mkDefault true;

  # Open firewall for Traefik and NFS
  networking.firewall = {
    allowedTCPPorts = [
      80
      443
      1883 # MQTT
      8883 # MQTTS
      8080 # Traefik metrics
      9000 # Node exporter
      9187 # PostgreSQL exporter
      # Exportarr exporters
      9707 # Sonarr
      9708 # Radarr
      9710 # Prowlarr
      9712 # Bazarr
      # NFS ports
      111 # rpcbind
      2049 # NFS daemon
      4000 # statd
      4001 # lockd
      4002 # mountd
      20048 # mountd (NFSv4)
    ];
    allowedUDPPorts = [
      # NFS ports
      111 # rpcbind
      2049 # NFS daemon
      4000 # statd
      4001 # lockd
      4002 # mountd
      20048 # mountd (NFSv4)
    ];
  };

  # Environment file for Gandi API key (managed by agenix)
  systemd.services.traefik.serviceConfig = {
    EnvironmentFile = config.age.secrets."gandi.env".path;
  };

  environment.systemPackages = with pkgs; [
    lm_sensors
    gnumake
    ffmpeg-full
  ];

}
