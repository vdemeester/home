# Generic NFS mounts module
# This module provides configurable NFS mounts for shared directories from any server
{
  config,
  lib,
  ...
}:
let
  cfg = config.services.nfs-mounts;

  # Helper to create mount configuration for a specific host
  mkHostMounts =
    hostName: hostCfg:
    lib.listToAttrs (
      map (folder: {
        name = "/net/${hostName}/${folder}";
        value = {
          device = "${hostCfg.server}:/${folder}"; # NFSv4: path relative to fsid=0
          fsType = "nfs";
          options = [
            "nfsvers=4.2" # Use NFSv4.2 for best performance
            "x-systemd.automount" # Lazy-mount on first access
            "noauto" # Don't mount at boot
            "x-systemd.idle-timeout=600" # Auto-unmount after 10 min idle
            "soft" # Don't hang if server unavailable
            "timeo=14" # Timeout after 1.4s (14 * 0.1s)
            "retrans=2" # Retry twice before timing out
            "_netdev" # Wait for network before mounting
          ];
        };
      }) hostCfg.folders
    );
in
{
  options.services.nfs-mounts = {
    hosts = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            server = lib.mkOption {
              type = lib.types.str;
              example = "rhea.sbr.pm";
              description = "NFS server hostname or IP";
            };
            folders = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              example = [
                "audiobooks"
                "music"
                "pictures"
              ];
              description = "List of folders to mount from the server's exported directory";
            };
          };
        }
      );
      default = { };
      example = {
        rhea = {
          server = "rhea.sbr.pm";
          folders = [
            "music"
            "audiobooks"
          ];
        };
        aion = {
          server = "aion.sbr.pm";
          folders = [
            "music"
            "audiobooks"
          ];
        };
      };
      description = ''
        NFS servers and their folders to mount.
        Each host's folders will be mounted at /net/{hostname}/{folder}.
      '';
    };
  };

  config = lib.mkIf (cfg.hosts != { }) {
    # Enable NFS support
    boot.supportedFilesystems = [ "nfs" ];

    # Create NFS mounts for all configured hosts
    fileSystems = lib.mkMerge (lib.mapAttrsToList mkHostMounts cfg.hosts);
  };
}
