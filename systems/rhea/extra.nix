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
    file = ../../secrets/rhea/gandi.env.age;
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
              middlewares = [ "syncthing-${name}-strip" ];
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
        in
        {
          http = {
            routers = syncthingRouters // {
              jellyfin = {
                rule = "Host(`jellyfin.sbr.pm`)";
                service = "jellyfin";
                entryPoints = [ "websecure" ];
                tls = {
                  certResolver = "letsencrypt";
                };
              };
              jellyseerr = {
                rule = "Host(`jellyseerr.sbr.pm`)";
                service = "jellyseerr";
                entryPoints = [ "websecure" ];
                tls = {
                  certResolver = "letsencrypt";
                };
              };
              sonarr = {
                rule = "Host(`sonarr.sbr.pm`)";
                service = "sonarr";
                entryPoints = [ "websecure" ];
                tls = {
                  certResolver = "letsencrypt";
                };
              };
              radarr = {
                rule = "Host(`radarr.sbr.pm`)";
                service = "radarr";
                entryPoints = [ "websecure" ];
                tls = {
                  certResolver = "letsencrypt";
                };
              };
              lidarr = {
                rule = "Host(`lidarr.sbr.pm`)";
                service = "lidarr";
                entryPoints = [ "websecure" ];
                tls = {
                  certResolver = "letsencrypt";
                };
              };
              bazarr = {
                rule = "Host(`bazarr.sbr.pm`)";
                service = "bazarr";
                entryPoints = [ "websecure" ];
                tls = {
                  certResolver = "letsencrypt";
                };
              };
              transmission = {
                rule = "Host(`transmission.sbr.pm`) || Host(`t.sbr.pm`)";
                service = "transmission";
                entryPoints = [ "websecure" ];
                tls = {
                  certResolver = "letsencrypt";
                };
              };
              immich = {
                rule = "Host(`immich.sbr.pm`)";
                service = "immich";
                entryPoints = [ "websecure" ];
                tls = {
                  certResolver = "letsencrypt";
                };
              };
              kiwix = {
                rule = "Host(`kiwix.sbr.pm`)";
                service = "kiwix";
                entryPoints = [ "websecure" ];
                tls = {
                  certResolver = "letsencrypt";
                };
              };
            };
            services = syncthingServices // {
              jellyfin = {
                loadBalancer = {
                  servers = [
                    { url = "http://localhost:8096"; }
                  ];
                };
              };
              jellyseerr = {
                loadBalancer = {
                  servers = [
                    { url = "http://localhost:5055"; }
                  ];
                };
              };
              sonarr = {
                loadBalancer = {
                  servers = [
                    { url = "http://localhost:8989"; }
                  ];
                };
              };
              radarr = {
                loadBalancer = {
                  servers = [
                    { url = "http://localhost:7878"; }
                  ];
                };
              };
              lidarr = {
                loadBalancer = {
                  servers = [
                    { url = "http://localhost:8686"; }
                  ];
                };
              };
              bazarr = {
                loadBalancer = {
                  servers = [
                    { url = "http://localhost:6767"; }
                  ];
                };
              };
              transmission = {
                loadBalancer = {
                  servers = [
                    { url = "http://localhost:9091"; }
                  ];
                };
              };
              immich = {
                loadBalancer = {
                  servers = [
                    { url = "http://localhost:2283"; }
                  ];
                };
              };
              kiwix = {
                loadBalancer = {
                  servers = [
                    { url = "http://${builtins.head globals.machines.sakhalin.net.vpn.ips}:8080"; }
                  ];
                };
              };
            };
            middlewares = syncthingMiddlewares;
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
      settings = {
        "backup" = {
          path = "/neo/backup";
          public = true;
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "yes";
          writable = true;
          comment = "backup";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "vincent";
          "force group" = "users";
        };
        "documents" = {
          path = "/neo/documents";
          public = true;
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "yes";
          writable = true;
          comment = "documents";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "vincent";
          "force group" = "users";
        };
        "downloads" = {
          path = "/neo/downloads";
          public = true;
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "yes";
          writable = true;
          comment = "downloads";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "vincent";
          "force group" = "users";
        };
        "music" = {
          path = "/neo/music";
          public = true;
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "yes";
          writable = true;
          comment = "music";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "vincent";
          "force group" = "users";
        };
        "pictures" = {
          path = "/neo/pictures";
          public = true;
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "yes";
          writable = true;
          comment = "pictures";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "vincent";
          "force group" = "users";
        };
        "videos" = {
          path = "/neo/videos";
          public = true;
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "yes";
          writable = true;
          comment = "videos";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "vincent";
          "force group" = "users";
        };
      };
    };
    nfs.server = {
      enable = true;
      exports = ''
                /neo                      192.168.1.0/24(rw,fsid=0,no_subtree_check) 10.100.0.0/24(rw,fsid=0,no_subtree_check)
                /neo/backup               192.168.1.0/24(rw,fsid=1,no_subtree_check) 10.100.0.0/24(rw,fsid=1,no_subtree_check)
                /neo/documents            192.168.1.0/24(rw,fsid=2,no_subtree_check) 10.100.0.0/24(rw,fsid=2,no_subtree_check)
                /neo/downloads            192.168.1.0/24(rw,fsid=2,no_subtree_check) 10.100.0.0/24(rw,fsid=2,no_subtree_check)
                /neo/music                192.168.1.0/24(rw,fsid=2,no_subtree_check) 10.100.0.0/24(rw,fsid=2,no_subtree_check)
                /neo/pictures             192.168.1.0/24(rw,fsid=2,no_subtree_check) 10.100.0.0/24(rw,fsid=2,no_subtree_check)
                /neo/videos               192.168.1.0/24(rw,fsid=2,no_subtree_check) 10.100.0.0/24(rw,fsid=2,no_subtree_check)
        			'';
    };
    immich = {
      enable = true;
      user = "vincent";
      group = "users";
      mediaLocation = "/neo/pictures/photos";
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
  };

  security.acme = {
    acceptTerms = true;
    email = "vincent@sbr.pm";
  };

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
