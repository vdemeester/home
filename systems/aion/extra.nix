{
  libx,
  globals,
  lib,
  pkgs,
  ...
}:
let
  # Service defaults for media/homelab services
  serviceDefaults = libx.mkServiceDefaults { };

  # Common rsync configuration for rhea backups
  rheaBackupDefaults = {
    source = {
      host = "rhea.sbr.pm";
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

  # Exportarr services configuration (data-driven approach)
  exportarrServices = {
    lidarr = {
      port = 9709;
      servicePort = 8686;
    };
  };
in
{
  imports = [
    ../common/services/prometheus-exporters-node.nix
    ../../modules/audible-sync
    ../../modules/music-playlist-dl
  ];

  users.users.vincent.linger = true;

  services = {
    wireguard = {
      enable = true;
      ips = libx.wg-ips globals.machines.aion.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };

    audible-sync = {
      enable = false; # enable one migration dayrs
      user = "vincent";
      outputDir = "/neo/audiobooks";
      tempDir = "/neo/audiobooks/zz_import"; # Keep AAX files for reuse
      quality = "best";
      format = "m4b";
      schedule = "daily"; # Run daily at 3 AM
      notification = {
        enable = true;
        ntfyUrl = "https://ntfy.sbr.pm";
        topic = "homelab";
      };
    };

    audiobookshelf = serviceDefaults // {
      enable = false;
      port = 13378;
      host = "0.0.0.0";
    };

    lidarr = serviceDefaults // {
      enable = false;
      settings.server.port = exportarrServices.lidarr.servicePort;
    };

    rsync-replica = {
      enable = true;
      jobs = {
        rhea-daily = rheaBackupDefaults // {
          source = rheaBackupDefaults.source // {
            paths = [
              "/neo/audiobooks"
              "/neo/documents"
              "/neo/ebooks"
            ];
          };
          schedule = "daily";
        };
        rhea-hourly = rheaBackupDefaults // {
          source = rheaBackupDefaults.source // {
            paths = [
              "/neo/music"
              "/neo/pictures"
              "/neo/videos"
            ];
          };
          schedule = "hourly";
        };
      };
    };

    music-playlist-dl = {
      enable = false; # Enable on music migration day
      user = "vincent";
      configFile = "/neo/music/music-playlist-dl.yaml";
      baseDir = "/neo/music/mixes"; # Downloads to /neo/music/mixes/{show}, playlists to /neo/music/playlists
      schedule = "weekly"; # Run weekly on Sundays at 2 AM
      notification = {
        enable = true;
        ntfyUrl = "https://ntfy.sbr.pm";
        topic = "homelab";
      };
    };

    navidrome = {
      enable = true;
      settings = {
        MusicFolder = "/neo/music";
        Address = "0.0.0.0";
        Port = 4533;
        BaseURL = "https://music.sbr.pm";

        # Paths
        DataFolder = "/var/lib/navidrome";
        CacheFolder = "/var/cache/navidrome";

        # Features
        EnableTranscodingConfig = true;
        EnableSubsonic = true;

        # Optional: Scrobbling (can enable later)
        # LastFM.Enabled = true;
      };
    };

    transmission = {
      enable = false; # Enable on music migration day
      package = pkgs.transmission_4;
      openRPCPort = true; # Open firewall for RPC (port 9091)
      home = "/neo/torrents";
      settings = {
        # Override default settings
        incomplete-dir-enabled = true;
        rpc-bind-address = "0.0.0.0"; # Bind to all interfaces
        rpc-host-whitelist = "localhost,tm.sbr.pm,transmission-music.sbr.pm,aion.home,aion.vpn,aion.sbr.pm,192.168.1.51,10.100.0.51";
        rpc-host-whitelist-enabled = true;
        rpc-whitelist-enabled = true;
        rpc-whitelist = "127.0.0.1,192.168.1.*,10.100.0.*"; # Allow local network access
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
  };

  networking = {
    useDHCP = lib.mkDefault true;
    firewall.allowedTCPPorts = [
      4533 # Navidrome
      9000 # Node exporter
      9091 # Transmission (music torrents)
    ];
  };

  environment.systemPackages = with pkgs; [
    lm_sensors
    gnumake
    audible-converter
    audible-cli
    ffmpeg-full
  ];

}
