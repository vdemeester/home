# NFS mounts from rhea server
# This module provides configurable NFS mounts for rhea's shared directories
{
  config,
  lib,
  ...
}:
let
  cfg = config.services.nfs-rhea-mounts;
in
{
  options.services.nfs-rhea-mounts = {
    enable = lib.mkEnableOption "NFS mounts from rhea server";

    folders = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [
        "audiobooks"
        "music"
        "pictures"
        "videos"
        "downloads"
      ];
      description = ''
        List of folders to mount from rhea's /neo directory.
        Each folder will be mounted at /net/rhea/{folder}.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # Enable NFS support
    boot.supportedFilesystems = [ "nfs" ];

    # Create NFS mounts for each folder
    fileSystems = lib.listToAttrs (
      map (folder: {
        name = "/net/rhea/${folder}";
        value = {
          device = "rhea.sbr.pm:/${folder}"; # NFSv4: path relative to fsid=0 (/neo)
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
      }) cfg.folders
    );
  };
}
