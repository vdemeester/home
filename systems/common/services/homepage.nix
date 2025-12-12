{
  lib,
  config,
  globals,
  ...
}:
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
              ping = "https://jellyfin.sbr.pm";
              statusStyle = "dot";
            };
          }
          {
            Jellyseerr = {
              description = "Media Requests";
              href = "https://jellyseerr.sbr.pm";
              icon = "jellyseerr.png";
              ping = "https://jellyseerr.sbr.pm";
              statusStyle = "dot";
            };
          }
          {
            Navidrome = {
              description = "Music Streaming";
              href = "https://music.sbr.pm";
              icon = "navidrome.png";
              ping = "https://music.sbr.pm";
              statusStyle = "dot";
            };
          }
          {
            Audiobookshelf = {
              description = "Podcasts & Audiobooks";
              href = "https://podcasts.sbr.pm";
              icon = "audiobookshelf.png";
              ping = "https://podcasts.sbr.pm";
              statusStyle = "dot";
            };
          }
          {
            Immich = {
              description = "Photo Management";
              href = "https://immich.sbr.pm";
              icon = "immich.png";
              ping = "https://immich.sbr.pm";
              statusStyle = "dot";
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
              ping = "https://sonarr.sbr.pm";
              statusStyle = "dot";
              widget = {
                type = "sonarr";
                url = "https://sonarr.sbr.pm";
                key = "{{HOMEPAGE_FILE_SONARR_KEY}}";
              };
            };
          }
          {
            Radarr = {
              description = "Movies";
              href = "https://radarr.sbr.pm";
              icon = "radarr.png";
              ping = "https://radarr.sbr.pm";
              statusStyle = "dot";
              widget = {
                type = "radarr";
                url = "https://radarr.sbr.pm";
                key = "{{HOMEPAGE_FILE_RADARR_KEY}}";
              };
            };
          }
          {
            Lidarr = {
              description = "Music";
              href = "https://lidarr.sbr.pm";
              icon = "lidarr.png";
              ping = "https://lidarr.sbr.pm";
              statusStyle = "dot";
              widget = {
                type = "lidarr";
                url = "https://lidarr.sbr.pm";
                key = "{{HOMEPAGE_FILE_LIDARR_KEY}}";
              };
            };
          }
          {
            Bazarr = {
              description = "Subtitles";
              href = "https://bazarr.sbr.pm";
              icon = "bazarr.png";
              ping = "https://bazarr.sbr.pm";
              statusStyle = "dot";
            };
          }
          {
            Prowlarr = {
              description = "Indexer Manager";
              href = "https://prowlarr.sbr.pm";
              icon = "prowlarr.png";
              ping = "https://prowlarr.sbr.pm";
              statusStyle = "dot";
            };
          }
          {
            Transmission = {
              description = "Torrent Client";
              href = "https://transmission.sbr.pm";
              icon = "transmission.png";
              ping = "https://transmission.sbr.pm";
              statusStyle = "dot";
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
              ping = "https://grafana.sbr.pm";
              statusStyle = "dot";
            };
          }
          {
            Healthchecks = {
              description = "Uptime Monitoring";
              href = "https://healthchecks.sbr.pm";
              icon = "healthchecks.png";
              ping = "https://healthchecks.sbr.pm";
              statusStyle = "dot";
            };
          }
          {
            "Home Assistant" = {
              description = "Home Automation";
              href = "https://home.sbr.pm";
              icon = "home-assistant.png";
              ping = "https://home.sbr.pm";
              statusStyle = "dot";
            };
          }
          {
            Traefik = {
              description = "Reverse Proxy";
              href = "https://traefik.sbr.pm";
              icon = "traefik.png";
              ping = "https://traefik.sbr.pm";
              statusStyle = "dot";
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
              ping = "https://paperless.sbr.pm";
              statusStyle = "dot";
            };
          }
          {
            N8N = {
              description = "Workflow Automation";
              href = "https://n8n.sbr.pm";
              icon = "n8n.png";
              ping = "https://n8n.sbr.pm";
              statusStyle = "dot";
            };
          }
          {
            Kiwix = {
              description = "Offline Wikipedia";
              href = "https://kiwix.sbr.pm";
              icon = "kiwix.png";
              ping = "https://kiwix.sbr.pm";
              statusStyle = "dot";
            };
          }
          {
            Linkwarden = {
              description = "Bookmark Manager";
              href = "https://links.sbr.pm";
              icon = "linkwarden.png";
              ping = "https://links.sbr.pm";
              statusStyle = "dot";
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
            "SourceHut" = [
              {
                abbr = "SH";
                href = "https://sr.ht/~vdemeester";
              }
            ];
          }
          {
            Codeberg = [
              {
                abbr = "CB";
                href = "https://codeberg.org/vdemeester";
              }
            ];
          }
        ];
      }
      {
        NixOS = [
          {
            "NixOS Search" = [
              {
                abbr = "NS";
                href = "https://search.nixos.org";
              }
            ];
          }
          {
            "NixOS Packages" = [
              {
                abbr = "NP";
                href = "https://search.nixos.org/packages";
              }
            ];
          }
          {
            "NixOS Options" = [
              {
                abbr = "NO";
                href = "https://search.nixos.org/options";
              }
            ];
          }
          {
            "Home Manager Options" = [
              {
                abbr = "HM";
                href = "https://home-manager-options.extranix.com";
              }
            ];
          }
          {
            "NixOS Wiki" = [
              {
                abbr = "NW";
                href = "https://wiki.nixos.org";
              }
            ];
          }
        ];
      }
      {
        Tools = [
          {
            "Claude Code" = [
              {
                abbr = "CC";
                href = "https://claude.ai/code";
              }
            ];
          }
          {
            "QMK Configurator" = [
              {
                abbr = "QMK";
                href = "https://config.qmk.fm";
              }
            ];
          }
          {
            "QMK Docs" = [
              {
                abbr = "QD";
                href = "https://docs.qmk.fm";
              }
            ];
          }
          {
            "Keymap Drawer" = [
              {
                abbr = "KD";
                href = "https://caksoylar.github.io/keymap-drawer";
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
          {
            "Tekton Docs" = [
              {
                abbr = "TK";
                href = "https://tekton.dev/docs/";
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

  # Create homepage group for secret file access
  users.groups.homepage = { };

  # Open firewall for local access
  networking.firewall.allowedTCPPorts = [ 3001 ];

  # Allow requests from all rhea domains and IPs (from globals.nix)
  systemd.services.homepage-dashboard = {
    environment = {
      HOMEPAGE_ALLOWED_HOSTS = lib.mkForce allowedHosts;
      HOMEPAGE_FILE_SONARR_KEY = config.age.secrets."exportarr-sonarr-apikey".path;
      HOMEPAGE_FILE_RADARR_KEY = config.age.secrets."exportarr-radarr-apikey".path;
      HOMEPAGE_FILE_LIDARR_KEY = config.age.secrets."exportarr-lidarr-apikey".path;
    };
    serviceConfig = {
      SupplementaryGroups = [ "homepage" ];
    };
  };
}
