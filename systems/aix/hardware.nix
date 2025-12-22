_: {
  imports = [
    ../common/services/nfs-mounts.nix
  ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
    "/data" = {
      device = "/dev/disk/by-label/data";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };

  # NFS mounts from rhea and aion
  services.nfs-mounts.hosts = {
    rhea = {
      server = "rhea.sbr.pm";
      folders = [
        "audiobooks"
        "downloads"
        "ebooks"
        "music"
        "pictures"
        "videos"
      ];
    };
    aion = {
      server = "aion.sbr.pm";
      folders = [
        "audiobooks"
        "music"
      ];
    };
  };
}
