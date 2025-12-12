{ lib, globals, ... }:
let
  rheaIPs = globals.machines.rhea.net.ips;
  rheaVPNIPs = globals.machines.rhea.net.vpn.ips;
  rheaDomains = globals.machines.rhea.net.names;

  # Build allowed hosts list from all rhea addresses
  allowedHosts = lib.concatStringsSep "," (
    rheaDomains
    ++ rheaIPs
    ++ rheaVPNIPs
    ++ [
      "127.0.0.1"
      "localhost"
      "homepage.sbr.pm"
    ]
  );
in
{
  # Homepage Dashboard - Homelab Services Overview
  # https://gethomepage.dev
  #
  # This module provides a centralized dashboard for monitoring homelab services.
  # Services are configured with widgets for real-time statistics where supported.
  #
  # TODO: Migrate Homepage to aion when that server is set up for web services

  services.homepage-dashboard = {
    enable = true;
    listenPort = 3001;

    settings = {
      title = "Homelab";
      favicon = "https://gethomepage.dev/img/favicon.ico";
      theme = "dark";
      color = "slate";

      layout = [
        {
          Media = {
            style = "row";
            columns = 3;
          };
        }
        {
          "Download Management" = {
            style = "row";
            columns = 3;
          };
        }
        {
          Infrastructure = {
            style = "row";
            columns = 2;
          };
        }
      ];
    };

    services = [
      {
        Media = [
          {
            Jellyfin = {
              description = "Media Server";
              href = "https://jellyfin.sbr.pm";
              icon = "jellyfin.png";
            };
          }
          {
            Jellyseerr = {
              description = "Media Requests";
              href = "https://jellyseerr.sbr.pm";
              icon = "jellyseerr.png";
            };
          }
          {
            Navidrome = {
              description = "Music Streaming";
              href = "https://navidrome.sbr.pm";
              icon = "navidrome.png";
            };
          }
          {
            Immich = {
              description = "Photo Management";
              href = "https://immich.sbr.pm";
              icon = "immich.png";
            };
          }
        ];
      }
      {
        "Download Management" = [
          {
            Sonarr = {
              description = "TV Shows";
              href = "https://sonarr.sbr.pm";
              icon = "sonarr.png";
            };
          }
          {
            Radarr = {
              description = "Movies";
              href = "https://radarr.sbr.pm";
              icon = "radarr.png";
            };
          }
          {
            Lidarr = {
              description = "Music";
              href = "https://lidarr.sbr.pm";
              icon = "lidarr.png";
            };
          }
          {
            Readarr = {
              description = "Books";
              href = "https://readarr.sbr.pm";
              icon = "readarr.png";
            };
          }
          {
            Bazarr = {
              description = "Subtitles";
              href = "https://bazarr.sbr.pm";
              icon = "bazarr.png";
            };
          }
          {
            Prowlarr = {
              description = "Indexer Manager";
              href = "https://prowlarr.sbr.pm";
              icon = "prowlarr.png";
            };
          }
          {
            Transmission = {
              description = "Torrent Client";
              href = "https://transmission.sbr.pm";
              icon = "transmission.png";
            };
          }
        ];
      }
      {
        Infrastructure = [
          {
            Grafana = {
              description = "Monitoring";
              href = "https://grafana.sbr.pm";
              icon = "grafana.png";
            };
          }
          {
            "Home Assistant" = {
              description = "Home Automation";
              href = "https://home.sbr.pm";
              icon = "home-assistant.png";
            };
          }
          {
            Traefik = {
              description = "Reverse Proxy";
              href = "https://traefik.sbr.pm";
              icon = "traefik.png";
            };
          }
        ];
      }
      {
        "Other Services" = [
          {
            Paperless = {
              description = "Document Management";
              href = "https://paperless.sbr.pm";
              icon = "paperless.png";
            };
          }
          {
            N8N = {
              description = "Workflow Automation";
              href = "https://n8n.sbr.pm";
              icon = "n8n.png";
            };
          }
          {
            Kiwix = {
              description = "Offline Wikipedia";
              href = "https://kiwix.sbr.pm";
              icon = "kiwix.png";
            };
          }
        ];
      }
    ];

    bookmarks = [
      {
        Development = [
          {
            GitHub = [
              {
                abbr = "GH";
                href = "https://github.com/vdemeester";
              }
            ];
          }
          {
            "NixOS Search" = [
              {
                abbr = "NX";
                href = "https://search.nixos.org";
              }
            ];
          }
        ];
      }
      {
        Documentation = [
          {
            "Homepage Docs" = [
              {
                abbr = "HP";
                href = "https://gethomepage.dev";
              }
            ];
          }
          {
            "Traefik Docs" = [
              {
                abbr = "TR";
                href = "https://doc.traefik.io/traefik/";
              }
            ];
          }
        ];
      }
    ];

    widgets = [
      {
        resources = {
          cpu = true;
          memory = true;
          disk = "/";
        };
      }
      {
        search = {
          provider = "duckduckgo";
          target = "_blank";
        };
      }
    ];
  };

  # Open firewall for local access
  networking.firewall.allowedTCPPorts = [ 3001 ];

  # Allow requests from all rhea domains and IPs (from globals.nix)
  systemd.services.homepage-dashboard.environment = {
    HOMEPAGE_ALLOWED_HOSTS = lib.mkForce allowedHosts;
  };
}
