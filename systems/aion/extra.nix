{
  libx,
  globals,
  lib,
  pkgs,
  config,
  ...
}:
{
  age.secrets."gandi.env" = {
    file = ../../secrets/aion/gandi.env.age;
    mode = "400";
    owner = "traefik";
    group = "traefik";
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

                # Services running on rhea
                jellyfin = mkRouter "jellyfin" [ "jellyfin.sbr.pm" ];
                jellyseerr = mkRouter "jellyseerr" [ "jellyseerr.sbr.pm" ];
                sonarr = mkRouter "sonarr" [ "sonarr.sbr.pm" ];
                radarr = mkRouter "radarr" [ "radarr.sbr.pm" ];
                lidarr = mkRouter "lidarr" [ "lidarr.sbr.pm" ];
                bazarr = mkRouter "bazarr" [ "bazarr.sbr.pm" ];
                transmission = mkRouter "transmission" [ "transmission.sbr.pm" "t.sbr.pm" ];

                # Services running on other hosts
                home = mkRouter "home" [ "home.sbr.pm" ];
                kiwix = mkRouter "kiwix" [ "kiwix.sbr.pm" ];
                n8n = mkRouter "n8n" [ "n8n.sbr.pm" ];
                paperless = mkRouter "paperless" [ "paperless.sbr.pm" ];
                grafana = mkRouter "grafana" [ "grafana.sbr.pm" ];
              };
            services =
              syncthingServices
              // localHttpServices
              // {
                # Services on rhea
                jellyfin = mkService "http://${builtins.head globals.machines.rhea.net.ips}:8096";
                jellyseerr = mkService "http://${builtins.head globals.machines.rhea.net.ips}:5055";
                sonarr = mkService "http://${builtins.head globals.machines.rhea.net.ips}:8989";
                radarr = mkService "http://${builtins.head globals.machines.rhea.net.ips}:7878";
                lidarr = mkService "http://${builtins.head globals.machines.rhea.net.ips}:8686";
                bazarr = mkService "http://${builtins.head globals.machines.rhea.net.ips}:6767";
                transmission = mkService "http://${builtins.head globals.machines.rhea.net.ips}:9091";

                # Services on other hosts
                home = mkService "http://${builtins.head globals.machines.athena.net.ips}:8080";
                kiwix = mkService "http://${builtins.head globals.machines.sakhalin.net.ips}:8080";
                n8n = mkService "http://${builtins.head globals.machines.sakhalin.net.ips}:5678";
                paperless = mkService "http://${builtins.head globals.machines.sakhalin.net.ips}:8000";
                grafana = mkService "http://${builtins.head globals.machines.sakhalin.net.ips}:3000";
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
      ips = libx.wg-ips globals.machines.aion.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
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
  };

  # Grant vincent ownership and superuser privileges for the immich database
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

  security.acme = {
    acceptTerms = true;
    defaults.email = "vincent@sbr.pm";
  };

  # Environment file for Gandi API key (managed by agenix)
  systemd.services.traefik.serviceConfig = {
    EnvironmentFile = config.age.secrets."gandi.env".path;
  };

  networking.useDHCP = lib.mkDefault true;

  # Open firewall for Traefik
  networking.firewall.allowedTCPPorts = [
    80
    443
    1883 # MQTT
    8883 # MQTTS
  ];

  environment.systemPackages = with pkgs; [
    lm_sensors
    gnumake
  ];

}
