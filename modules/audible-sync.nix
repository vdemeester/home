{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.audible-sync;
in
{
  options.services.audible-sync = {
    enable = mkEnableOption "Audible to Audiobookshelf sync service";

    user = mkOption {
      type = types.str;
      default = "vincent";
      description = "User to run the sync service as";
    };

    outputDir = mkOption {
      type = types.str;
      default = "/neo/audiobooks";
      description = "Output directory for converted audiobooks";
    };

    tempDir = mkOption {
      type = types.str;
      default = "/neo/audiobooks/zz_import";
      description = "Temporary directory for downloads";
    };

    quality = mkOption {
      type = types.enum [
        "best"
        "high"
        "normal"
      ];
      default = "best";
      description = "Audio quality for downloads";
    };

    format = mkOption {
      type = types.enum [
        "m4b"
        "mp3"
        "m4a"
      ];
      default = "m4b";
      description = "Output format for converted audiobooks";
    };

    schedule = mkOption {
      type = types.str;
      default = "daily";
      description = "Systemd timer schedule (daily, weekly, etc.)";
    };

    time = mkOption {
      type = types.str;
      default = "03:00";
      description = "Time of day to run sync (24-hour format)";
    };

    onCalendar = mkOption {
      type = types.str;
      default = "*-*-* 03:00:00";
      description = "Full OnCalendar specification for systemd timer";
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
        default = "audible-sync";
        description = "ntfy topic for notifications";
      };
    };
  };

  config = mkIf cfg.enable {
    # Install the converter tool
    environment.systemPackages = with pkgs; [
      audible-converter
    ];

    # Systemd timer for scheduled sync
    systemd.timers.audible-sync = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = cfg.onCalendar;
        Persistent = true;
        RandomizedDelaySec = "10m";
      };
    };

    # Systemd service to run the sync
    systemd.services.audible-sync = {
      description = "Sync Audible library to Audiobookshelf";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];

      environment = {
        AUDIBLE_OUTPUT_DIR = cfg.outputDir;
        AUDIBLE_TEMP_DIR = cfg.tempDir;
        AUDIBLE_QUALITY = cfg.quality;
        AUDIBLE_FORMAT = cfg.format;
        HOME = "/home/${cfg.user}";
      };

      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        Group = "users";

        # Run the sync command
        ExecStart = "${pkgs.audible-converter}/bin/audible-converter sync";

        # Notifications on success (if enabled)
        ExecStartPost = mkIf cfg.notification.enable (
          pkgs.writeShellScript "audible-sync-notify-success" ''
            ${pkgs.curl}/bin/curl -H "Title: Audible Sync Complete" \
              -H "Tags: white_check_mark,books" \
              -d "Successfully synced Audible library to ${cfg.outputDir}" \
              ${cfg.notification.ntfyUrl}/${cfg.notification.topic}
          ''
        );

        # Ensure directories exist
        ExecStartPre = pkgs.writeShellScript "audible-sync-prepare" ''
          mkdir -p "${cfg.outputDir}"
          mkdir -p "${cfg.tempDir}"
        '';

        # Note: We keep tempDir intact to reuse downloaded AAX files
        # and reduce bandwidth usage on subsequent runs

        # Resource limits
        Nice = 15;
        IOSchedulingClass = "idle";
        CPUSchedulingPolicy = "idle";

        # Security hardening
        PrivateTmp = true;
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = "read-only";
        ReadWritePaths = [
          cfg.outputDir
          cfg.tempDir
        ];
      };

      # Notify on failure (if enabled)
      onFailure = mkIf cfg.notification.enable [ "audible-sync-failure.service" ];
    };

    # Failure notification service
    systemd.services.audible-sync-failure = mkIf cfg.notification.enable {
      description = "Notify on Audible sync failure";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "audible-sync-notify-failure" ''
          ${pkgs.curl}/bin/curl -H "Title: Audible Sync Failed" \
            -H "Priority: high" \
            -H "Tags: warning,books" \
            -d "Audible sync failed. Check logs: journalctl -u audible-sync" \
            ${cfg.notification.ntfyUrl}/${cfg.notification.topic}
        '';
      };
    };
  };
}
