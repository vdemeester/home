{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.rsync-replica;

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

  # Generate systemd service for a single job
  mkReplicaService = name: jobCfg: {
    description = "Rsync replica job: ${name}";
    serviceConfig = {
      Type = "oneshot";
      User = jobCfg.user;
      Group = jobCfg.group;
    };
    script =
      let
        # Build rsync command for each source path
        syncCommands = map (
          sourcePath:
          let
            # Extract the basename for destination (e.g., /neo/videos -> videos)
            basename = baseNameOf sourcePath;
            destPath = "${jobCfg.destination}/${basename}";

            # Base rsync arguments
            baseArgs = [
              "-aAX" # archive mode with ACLs and xattrs
              "--verbose"
              "--human-readable"
              "--progress"
            ]
            ++ optional jobCfg.delete "--delete"
            ++ optional jobCfg.deleteExcluded "--delete-excluded"
            ++ jobCfg.rsyncArgs;

            # SSH command with custom args
            sshCmd = "ssh ${concatStringsSep " " jobCfg.sshArgs}";

            # Full rsync command
            rsyncArgs = concatStringsSep " " (
              baseArgs
              ++ [
                "-e '${sshCmd}'"
                "${jobCfg.source.user}@${jobCfg.source.host}:${sourcePath}/"
                "${destPath}/"
              ]
            );
          in
          ''
            echo "Syncing ${sourcePath} from ${jobCfg.source.host} to ${destPath}"
            mkdir -p "${destPath}"
            ${pkgs.rsync}/bin/rsync ${rsyncArgs}
          ''
        ) jobCfg.source.paths;
      in
      concatStringsSep "\n" syncCommands;

    path = with pkgs; [
      openssh
      rsync
    ];
  };

  # Generate systemd timer for a single job
  mkReplicaTimer = name: jobCfg: {
    description = "Timer for rsync replica job: ${name}";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = scheduleToCalendar jobCfg.schedule;
      Persistent = true;
      RandomizedDelaySec = jobCfg.randomizedDelay;
    };
  };

in
{
  options.services.rsync-replica = {
    enable = mkEnableOption "rsync-based replication service";

    jobs = mkOption {
      type = types.attrsOf (
        types.submodule {
          options = {
            source = {
              host = mkOption {
                type = types.str;
                description = "Remote hostname to sync from";
                example = "rhea";
              };

              user = mkOption {
                type = types.str;
                default = "root";
                description = "SSH user to connect as";
              };

              paths = mkOption {
                type = types.listOf types.str;
                description = "List of absolute paths on the source host to sync";
                example = [
                  "/neo/videos"
                  "/neo/pictures"
                ];
              };
            };

            destination = mkOption {
              type = types.str;
              description = "Local destination directory (source basenames will be created here)";
              example = "/neo";
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

            delete = mkOption {
              type = types.bool;
              default = true;
              description = "Delete files in destination that don't exist in source (true replica)";
            };

            deleteExcluded = mkOption {
              type = types.bool;
              default = false;
              description = "Also delete excluded files from destination";
            };

            rsyncArgs = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = "Additional arguments to pass to rsync";
              example = [
                "--exclude=*.tmp"
                "--bwlimit=10000"
              ];
            };

            sshArgs = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = "Additional arguments to pass to SSH";
              example = [
                "-p 2222"
                "-i /root/.ssh/id_ed25519"
              ];
            };

            user = mkOption {
              type = types.str;
              default = "root";
              description = "User to run the rsync job as";
            };

            group = mkOption {
              type = types.str;
              default = "root";
              description = "Group to run the rsync job as";
            };

            randomizedDelay = mkOption {
              type = types.str;
              default = "0";
              description = "Randomized delay before starting the job (systemd RandomizedDelaySec)";
              example = "1h";
            };
          };
        }
      );
      default = { };
      description = "Rsync replication jobs to run";
    };
  };

  config = mkIf cfg.enable {
    systemd.services = mapAttrs' (
      name: jobCfg: nameValuePair "rsync-replica-${name}" (mkReplicaService name jobCfg)
    ) cfg.jobs;

    systemd.timers = mapAttrs' (
      name: jobCfg: nameValuePair "rsync-replica-${name}" (mkReplicaTimer name jobCfg)
    ) cfg.jobs;
  };
}
