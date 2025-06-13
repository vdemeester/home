_: {
  disko.devices = {
    disk = {
      # 512GB root/boot drive. Configured with:
      # - A FAT32 ESP partition for systemd-boot
      # - A LUKS container which contains an EXT4 filesystem
      root = {
        type = "disk";
        device = ""; # FIXME
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
                # LUKS passphrase will be prompted interactively only
                type = "luks";
                name = "cryptroot";
                askPassword = true;
                settings = {
                  allowDiscards = true;
                };
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
  };
}
