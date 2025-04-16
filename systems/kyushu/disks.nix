{ lib
, disks ? [
    "/dev/nvme0n1"
  ]
, ...
}:
{

  boot.initrd.luks.devices."root" = {
    # FIXME setup this
    # TODO: Remove this "device" attr if/when machine is reinstalled.
    # This is a workaround for the legacy -> gpt tables disko format.
    # device = lib.mkForce "/dev/disk/by-uuid/c0cac87c-53ec-4262-9ab2-a3ee8331c75a";
    preLVM = true;
    allowDiscards = true;
    keyFile = "/dev/disk/by-id/usb-_USB_DISK_2.0_070D375D84327E87-0:0";
    keyFileOffset = 30992883712;
    keyFileSize = 4096;
    # fallbackToPassword = lib.mkForce true;
  };

  # TODO: Remove this if/when machine is reinstalled.
  # This is a workaround for the legacy -> gpt tables disko format.
  fileSystems."/boot".device = lib.mkForce "/dev/disk/by-partlabel/ESP";

  disko.devices = {
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
                # LUKS passphrase will be prompted interactively only
                type = "luks";
                name = "crypted";
                settings = {
                  # Make sure there is no trailing newline in keyfile if used for interactive unlock.
                  # Use `echo -n "password" > /tmp/data.keyfile`
                  keyFile = "/tmp/data.keyfile";
                  allowDiscards = true;
                };
                content = {
                  type = "filesystem";
                  format = "ext4";
                  mountpoint = "/";
                  mountOptions = [ "noatime" "nodiratime" "discard" ];
                };
              };
            };
          };
        };
      };
    };
  };
}
