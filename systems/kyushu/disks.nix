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
      nvme0 = {
        device = "/dev/nvme0n1";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              start = "0%";
              size = "512MiB";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                # mountOptions = [ "umask=0077" ];
              };
            };
            luks = {
              start = "512MiB";
              size = "100%";
              content = {
                type = "luks";
                name = "root";
                settings.allowDiscards = true;
                passwordFile = "/tmp/secret.key";
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
