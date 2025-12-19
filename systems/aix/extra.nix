{
  lib,
  libx,
  globals,
  ...
}:
let
  # Rhea's VPN IP for TLS proxy backend
  rheaVpnIP = lib.head globals.machines.rhea.net.vpn.ips;

  # Aix's local IP for DNS resolution
  aixLocalIP = "192.168.1.75";

  # Common rsync configuration for aion sync
  aionSyncDefaults = {
    source = {
      host = "aion.sbr.pm";
      user = "vincent";
    };
    destination = "/data";
    delete = true; # Mirror mode: delete files in destination that don't exist in source
    user = "vincent";
    group = "users";
    rsyncArgs = [
      "--exclude=.Trash-*"
      "--exclude=lost+found"
      "--exclude=.stfolder"
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
  ];

  networking.firewall.enable = false;

  # TODO make it an option ? (otherwise I'll add it for all)
  users.users.vincent.linger = true;

  services = {
    # Rsync data from aion to aix for local network access
    rsync-replica = {
      enable = true;
      jobs = {
        # Sync all data daily
        aion-daily = aionSyncDefaults // {
          source = aionSyncDefaults.source // {
            paths = [
              "/neo/music"
              "/neo/pictures"
              "/neo/ebooks"
              "/neo/audiobooks"
            ];
          };
          schedule = "daily";
        };
      };
    };

    samba.settings = {
      global."server string" = "Aix";
      vincent = {
        path = "/data/share";
        public = "yes";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        writable = "yes";
        comment = "Vincent's share";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "vincent";
        "force group" = "users";
      };
    };
    wireguard = {
      enable = true;
      ips = libx.wg-ips globals.machines.aix.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };

    # DNS resolver for local network - resolve specific sbr.pm domains to Aix
    dnsmasq = {
      enable = true;
      settings = {
        # Listen on local network interface
        interface = "end0";
        bind-dynamic = true;

        # DNS settings
        domain-needed = true;
        bogus-priv = true;

        # Resolve specific media service domains to Aix (which will reverse proxy)
        address = [
          "/music.sbr.pm/${aixLocalIP}"
          "/navidrome.sbr.pm/${aixLocalIP}"
          "/jellyfin.sbr.pm/${aixLocalIP}"
          "/podcasts.sbr.pm/${aixLocalIP}"
          "/audiobookshelf.sbr.pm/${aixLocalIP}"
          "/immich.sbr.pm/${aixLocalIP}"
        ];

        # Use upstream DNS for other queries
        server = [
          "1.1.1.1"
          "8.8.8.8"
        ];

        # Cache settings
        cache-size = 1000;
      };
    };

    # Nginx TCP/TLS pass-through to rhea's Traefik
    nginx = {
      enable = true;

      # Enable stream module for TCP/TLS proxying
      streamConfig = ''
        # Map SNI hostname to backend
        # All services go to rhea's Traefik, which routes internally
        map $ssl_preread_server_name $backend {
          navidrome.sbr.pm      ${rheaVpnIP}:443;
          music.sbr.pm          ${rheaVpnIP}:443;
          jellyfin.sbr.pm       ${rheaVpnIP}:443;
          audiobookshelf.sbr.pm ${rheaVpnIP}:443;
          podcasts.sbr.pm       ${rheaVpnIP}:443;
          immich.sbr.pm         ${rheaVpnIP}:443;
          default               ${rheaVpnIP}:443;
        }

        # HTTPS proxy server
        server {
          listen 443;
          listen [::]:443;

          # Read SNI without terminating TLS
          ssl_preread on;

          # Forward to rhea's Traefik
          proxy_pass $backend;

          # Connection settings for streaming
          proxy_connect_timeout 5s;
          proxy_timeout 24h;
          proxy_buffer_size 16k;
        }
      '';
    };
  };
}
