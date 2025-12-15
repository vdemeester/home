_: {
  imports = [
    ../common/services/nfs-rhea-mounts.nix
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

  # NFS mounts from rhea
  services.nfs-rhea-mounts = {
    enable = true;
    folders = [
      "audiobooks"
      "downloads"
      "ebooks"
      "music"
      "pictures"
      "videos"
    ];
  };
}
