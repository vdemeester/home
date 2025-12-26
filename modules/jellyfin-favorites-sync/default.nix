{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.jellyfin-favorites-sync;

  # Convert schedule shortcuts to systemd OnCalendar format
  scheduleToCalendar =
    schedule:
    if schedule == "hourly" then
      "hourly"
    else if schedule == "daily" then
      "daily"
    else if schedule == "weekly" then
      "weekly"
    else
      schedule;

in
{
  options.services.jellyfin-favorites-sync = {
    enable = mkEnableOption "Jellyfin favorites sync service";

    package = mkOption {
      type = types.package;
      default = pkgs.jellyfin-favorites-sync;
      defaultText = literalExpression "pkgs.jellyfin-favorites-sync";
      description = "The jellyfin-favorites-sync package to use.";
    };

    schedule = mkOption {
      type = types.str;
      default = "daily";
      description = ''
        When to run the sync. Can be "hourly", "daily", "weekly", or a systemd OnCalendar format.
        See systemd.time(7) for OnCalendar format details.
      '';
      example = "daily";
    };

    jellyfinUrl = mkOption {
      type = types.str;
      example = "https://jellyfin.sbr.pm";
      description = "Jellyfin server URL";
    };

    apiKeyFile = mkOption {
      type = types.path;
      description = "Path to file containing Jellyfin API key (managed by agenix)";
    };

    userId = mkOption {
      type = types.str;
      description = "Jellyfin user ID or username (will be auto-resolved to GUID)";
    };

    sourceRoot = mkOption {
      type = types.str;
      default = "/neo/videos";
      description = "Root path of Jellyfin library on source host";
    };

    destination = {
      host = mkOption {
        type = types.str;
        default = "aix.sbr.pm";
        description = "Target SSH host for rsync";
      };

      user = mkOption {
        type = types.str;
        default = "vincent";
        description = "SSH user for remote connection";
      };

      root = mkOption {
        type = types.str;
        default = "/data/favorites";
        description = "Destination path on target host";
      };
    };

    sshArgs = mkOption {
      type = types.listOf types.str;
      default = [ "-o StrictHostKeyChecking=accept-new" ];
      description = "Additional SSH arguments";
      example = [
        "-p 2222"
        "-i /home/jellyfin-favorites-sync/.ssh/id_ed25519"
      ];
    };

    user = mkOption {
      type = types.str;
      default = "jellyfin-favorites-sync";
      description = "System user to run service as";
    };

    group = mkOption {
      type = types.str;
      default = "jellyfin-favorites-sync";
      description = "System group to run service as";
    };

    randomizedDelay = mkOption {
      type = types.str;
      default = "5m";
      description = "Randomized delay before starting the job (systemd RandomizedDelaySec)";
      example = "1h";
    };
  };

  config = mkIf cfg.enable {
    # Create system user/group
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.group;
      description = "Jellyfin favorites sync service user";
    };

    users.groups.${cfg.group} = { };

    # Systemd service
    systemd.services.jellyfin-favorites-sync = {
      description = "Jellyfin Favorites Sync";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];

      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        Group = cfg.group;

        ExecStart = pkgs.writeShellScript "jellyfin-favorites-sync-start" ''
          set -euo pipefail

          # Read API key from file
          API_KEY=$(cat ${cfg.apiKeyFile})

          # Execute sync
          ${cfg.package}/bin/jellyfin-favorites-sync \
            --jellyfin-url "${cfg.jellyfinUrl}" \
            --api-key "$API_KEY" \
            --user-id "${cfg.userId}" \
            --source-root "${cfg.sourceRoot}" \
            --dest-host "${cfg.destination.host}" \
            --dest-user "${cfg.destination.user}" \
            --dest-root "${cfg.destination.root}" \
            ${concatMapStringsSep " " (arg: "--ssh-arg '${arg}'") cfg.sshArgs} \
            --verbose
        '';

        # Security hardening
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        NoNewPrivileges = true;
        ReadWritePaths = [ "/tmp" ]; # For rsync file lists

        # Resource limits
        Nice = 15;
        IOSchedulingClass = "idle";
      };

      path = with pkgs; [
        openssh
        rsync
      ];
    };

    # Systemd timer
    systemd.timers.jellyfin-favorites-sync = {
      description = "Timer for Jellyfin Favorites Sync";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnCalendar = scheduleToCalendar cfg.schedule;
        Persistent = true;
        RandomizedDelaySec = cfg.randomizedDelay;
      };
    };
  };
}
