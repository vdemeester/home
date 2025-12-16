{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.music-playlist-dl;
in
{
  options.services.music-playlist-dl = {
    enable = mkEnableOption "Music playlist downloader service";

    user = mkOption {
      type = types.str;
      default = "vincent";
      description = "User to run the downloader service as";
    };

    group = mkOption {
      type = types.str;
      default = "users";
      description = "Group to run the downloader service as";
    };

    configFile = mkOption {
      type = types.path;
      default = "/neo/music/music-playlist-dl.yaml";
      description = "Path to YAML configuration file";
    };

    baseDir = mkOption {
      type = types.str;
      default = "/neo/music";
      description = "Base directory for downloads (library and playlists subdirectories)";
    };

    interval = mkOption {
      type = types.enum [
        "hourly"
        "daily"
        "weekly"
        "monthly"
      ];
      default = "weekly";
      description = "How often to run the downloader";
    };

    onCalendar = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Custom OnCalendar specification for systemd timer (overrides interval)";
      example = "Sun *-*-* 02:00:00";
    };

    notification = {
      enable = mkEnableOption "notifications via ntfy";

      ntfyUrl = mkOption {
        type = types.str;
        default = "https://ntfy.sbr.pm";
        description = "URL of ntfy server";
      };

      topic = mkOption {
        type = types.str;
        default = "homelab";
        description = "ntfy topic for notifications";
      };
    };
  };

  config = mkIf cfg.enable {
    # Install the music-playlist-dl tool
    environment.systemPackages = with pkgs; [
      music-playlist-dl
      yt-dlp
    ];

    # Systemd timer for scheduled downloads
    systemd.timers.music-playlist-dl = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar =
          if cfg.onCalendar != null then
            cfg.onCalendar
          else
            (
              {
                hourly = "*-*-* *:00:00";
                daily = "*-*-* 02:00:00";
                weekly = "Sun *-*-* 02:00:00";
                monthly = "*-*-01 02:00:00";
              }
              .${cfg.interval}
            );
        Persistent = true;
        RandomizedDelaySec = "15m";
      };
    };

    # Systemd service to run the downloader
    systemd.services.music-playlist-dl = {
      description = "Download music podcasts and generate playlists";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];

      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        Group = cfg.group;

        # Run the downloader command
        ExecStart = "${pkgs.music-playlist-dl}/bin/music-playlist-dl --config ${cfg.configFile}";

        # Notifications on success (if enabled)
        ExecStartPost = mkIf cfg.notification.enable (
          pkgs.writeShellScript "music-playlist-dl-notify-success" ''
            ${pkgs.curl}/bin/curl -H "Title: Music Playlist Download Complete" \
              -H "Tags: musical_note,headphones" \
              -d "Successfully downloaded music podcasts and updated playlists" \
              ${cfg.notification.ntfyUrl}/${cfg.notification.topic}
          ''
        );

        # Ensure directories exist
        ExecStartPre = pkgs.writeShellScript "music-playlist-dl-prepare" ''
          mkdir -p "${cfg.baseDir}/library"
          mkdir -p "${cfg.baseDir}/playlist"
        '';

        # Resource limits
        Nice = 15;
        IOSchedulingClass = "idle";
        CPUSchedulingPolicy = "idle";

        # Security hardening
        PrivateTmp = true;
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = "read-only";
        ReadWritePaths = [ cfg.baseDir ];
      };

      # Notify on failure (if enabled)
      onFailure = mkIf cfg.notification.enable [ "music-playlist-dl-failure.service" ];
    };

    # Failure notification service
    systemd.services.music-playlist-dl-failure = mkIf cfg.notification.enable {
      description = "Notify on music playlist download failure";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "music-playlist-dl-notify-failure" ''
          ${pkgs.curl}/bin/curl -H "Title: Music Playlist Download Failed" \
            -H "Priority: high" \
            -H "Tags: warning,musical_note" \
            -d "Music playlist download failed. Check logs: journalctl -u music-playlist-dl" \
            ${cfg.notification.ntfyUrl}/${cfg.notification.topic}
        '';
      };
    };
  };
}
