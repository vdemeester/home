{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.nix-flake-updater;

  # Use the nix-flake-update package
  updateScript = pkgs.writeShellScript "nix-flake-update-wrapper" ''
    export REPO_PATH="${cfg.repoPath}"
    export FLAKE_PATH="${cfg.flakePath}"
    export GIT_REMOTE="${cfg.gitRemote}"
    export BRANCH_PREFIX="${cfg.branchPrefix}"
    export NTFY_TOPIC="${cfg.ntfyTopic}"
    export NTFY_SERVER="${cfg.ntfyServer}"
    export BUILD_SYSTEMS="${toString cfg.buildSystems}"
    export DRY_RUN="${toString cfg.dryRun}"

    # Execute the packaged update script (already has tools in PATH)
    exec ${pkgs.nix-flake-update}/bin/nix-flake-update
  '';

in
{
  options.services.nix-flake-updater = {
    enable = mkEnableOption "automated Nix flake.lock updates";

    repoPath = mkOption {
      type = types.str;
      example = "/home/user/nixos-config";
      description = "Path to the git repository containing the flake";
    };

    flakePath = mkOption {
      type = types.str;
      default = cfg.repoPath;
      example = "/home/user/nixos-config";
      description = "Path to the flake (usually same as repoPath)";
    };

    gitRemote = mkOption {
      type = types.str;
      default = "origin";
      description = "Git remote name to push to";
    };

    branchPrefix = mkOption {
      type = types.str;
      default = "flake-update-";
      description = "Prefix for update branches";
    };

    buildSystems = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = [
        "aomi"
        "sakhalin"
      ];
      description = "List of NixOS systems to build for verification";
    };

    schedule = mkOption {
      type = types.str;
      default = "weekly";
      example = "Mon *-*-* 02:00:00";
      description = "Systemd timer schedule (OnCalendar format or 'weekly'/'daily')";
    };

    ntfyTopic = mkOption {
      type = types.str;
      default = "nix-updates";
      description = "ntfy topic for notifications";
    };

    ntfyServer = mkOption {
      type = types.str;
      default = "https://ntfy.sh";
      example = "http://ntfy.sbr.pm";
      description = "ntfy server URL";
    };

    dryRun = mkOption {
      type = types.bool;
      default = false;
      description = "If true, don't push to remote (testing mode)";
    };

    user = mkOption {
      type = types.str;
      default = "root";
      description = "User to run the update as";
    };

    randomizedDelaySec = mkOption {
      type = types.int;
      default = 3600;
      description = "Random delay in seconds before starting (0-value)";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.nix-flake-updater = {
      description = "Automated Nix flake.lock updater";

      serviceConfig = {
        Type = "oneshot";
        User = cfg.user;
        ExecStart = "${updateScript}";

        # Security hardening
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = "read-only";
        ReadWritePaths = [
          cfg.repoPath
          "/var/log/nix-flake-updater"
        ];
        NoNewPrivileges = true;

        # Logging
        StandardOutput = "journal";
        StandardError = "journal";
        SyslogIdentifier = "nix-flake-updater";
      };

      # Don't fail if update fails (e.g., no changes, build failures)
      unitConfig = {
        SuccessExitStatus = "0 1";
      };
    };

    systemd.timers.nix-flake-updater = {
      description = "Timer for automated Nix flake.lock updates";
      wantedBy = [ "timers.target" ];

      timerConfig = {
        OnCalendar = cfg.schedule;
        RandomizedDelaySec = cfg.randomizedDelaySec;
        Persistent = true;
      };
    };

    # Ensure log directory exists
    systemd.tmpfiles.rules = [
      "d /var/log/nix-flake-updater 0750 ${cfg.user} ${config.users.users.${cfg.user}.group} -"
    ];
  };
}
