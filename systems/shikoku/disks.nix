_: {
  disko.devices = {
    # NAME        MAJ:MIN RM   SIZE RO TYPE MOUNTPOINTS
    # sda           8:0    0   489G  0 disk
    # sdb           8:16   0   1.8T  0 disk
    # sdc           8:32   0 465.8G  0 disk
    # nvme0n1     259:0    0 232.9G  0 disk
    # nvme1n1     259:3    0 232.9G  0 disk
    disk = {
      # 512GB root/boot drive. Configured with:
      # - A FAT32 ESP partition for systemd-boot
      # - A LUKS container which contains an EXT4 filesystem
      root = {
        type = "disk";
        device = "/dev/nvme0n1";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              size = "1G";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" ];
              };
            };
            root = {
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/";
                mountOptions = [
                  "noatime"
                  "nodiratime"
                  "discard"
                ];
              };
            };
          };
        };
      };
    };
  };
}
