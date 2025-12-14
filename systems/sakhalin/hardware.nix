{
  ...
}:
{
  imports = [
    ../common/hardware/acpid.nix
    ../common/services/nfs-rhea-mounts.nix
  ];
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/92ce650d-873e-41c1-a44e-71c2b9191b9d";
    fsType = "ext4";
    options = [
      "noatime"
      "discard"
    ];
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/B226-075A";
    fsType = "vfat";
  };
  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/4f614c00-d94d-42f9-8386-3ecd396aa246";
    fsType = "ext4";
    options = [
      "noatime"
      "discard"
    ];
  };
  fileSystems."/mnt/gaia" = {
    device = "/dev/disk/by-uuid/88d3d686-d451-4ba9-bd6e-373601ed2683";
    fsType = "ext4";
    options = [ "noatime" ];
  };
  fileSystems."/mnt/toshito" = {
    device = "/dev/disk/by-uuid/3c7cf84e-2486-417d-9de8-4b7757d483e4";
    fsType = "ext4";
    options = [ "noatime" ];
  };

  fileSystems."/export/gaia" = {
    device = "/mnt/gaia";
    options = [ "bind" ];
  };
  fileSystems."/export/toshito" = {
    device = "/mnt/toshito";
    options = [ "bind" ];
  };

  swapDevices = [ { device = "/dev/disk/by-uuid/9eb067d1-b329-4fbb-ae27-38abfbe7c108"; } ];

  # NFS mounts from rhea
  services.nfs-rhea-mounts = {
    enable = true;
    folders = [
      "audiobooks"
      "downloads"
      "music"
      "pictures"
      "videos"
    ];
  };

  networking = {
    firewall.enable = false; # we are in safe territory :D
    bridges.br1.interfaces = [ "enp0s31f6" ];
    useDHCP = false;
    interfaces.br1 = {
      useDHCP = true;
    };
  };
}
