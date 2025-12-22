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
    ../common/services/samba.nix
    ../common/services/homepage.nix
    ../common/services/prometheus-exporters-node.nix
    ../../modules/audible-sync
    ../../modules/music-playlist-dl
  ];

  users.users.vincent.linger = true;

  # Age secrets for homepage widgets (API keys for *arr services on rhea)
  age.secrets = {
    "exportarr-sonarr-apikey" = {
      file = ../../secrets/rhea/exportarr-sonarr-apikey.age;
      mode = "440";
      group = "homepage";
    };
    "exportarr-radarr-apikey" = {
      file = ../../secrets/rhea/exportarr-radarr-apikey.age;
      mode = "440";
      group = "homepage";
    };
    "exportarr-lidarr-apikey" = {
      file = ../../secrets/rhea/exportarr-lidarr-apikey.age;
      mode = "440";
      group = "homepage";
    };
  };

  services = {
    wireguard = {
      enable = true;
      ips = libx.wg-ips globals.machines.aion.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };

    audible-sync = {
      enable = true; # enable one migration dayrs
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
              "/neo/documents"
              "/neo/ebooks"
            ];
          };
          schedule = "daily";
        };
        rhea-hourly = rheaBackupDefaults // {
          source = rheaBackupDefaults.source // {
            paths = [
              "/neo/pictures"
              "/neo/videos"
            ];
          };
          schedule = "hourly";
        };
      };
    };

    music-playlist-dl = {
      enable = true; # Enable on music migration day
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
      enable = true; # Enable on music migration day
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

    # Samba shares for music and audiobooks
    samba.settings = {
      global."server string" = "Aion";
      music = libx.mkSambaShare {
        name = "music";
        path = "/neo/music";
      };
      audiobooks = libx.mkSambaShare {
        name = "audiobooks";
        path = "/neo/audiobooks";
      };
    };

    # NFS server for music and audiobooks
    nfs.server = {
      enable = true;
      # Fixed ports for firewall configuration
      lockdPort = 4001;
      mountdPort = 4002;
      statdPort = 4000;
      exports = ''
        /neo/music              192.168.1.0/24(rw,fsid=0,no_subtree_check) 10.100.0.0/24(rw,fsid=0,no_subtree_check)
        /neo/audiobooks         192.168.1.0/24(rw,fsid=1,no_subtree_check) 10.100.0.0/24(rw,fsid=1,no_subtree_check)
      '';
    };
  };

  networking = {
    useDHCP = lib.mkDefault true;
    firewall = {
      allowedTCPPorts = [
        3001 # Homepage dashboard
        4533 # Navidrome
        13378 # Audiobookshelf
        9000 # Node exporter
        9709 # Lidarr
        9091 # Transmission (music torrents)
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
  };

  environment.systemPackages = with pkgs; [
    lm_sensors
    gnumake
    audible-converter
    audible-cli
    ffmpeg-full
  ];

}
  a
