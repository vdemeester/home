{ inputs, lib, ... }:
let
  firmwarePartition = lib.recursiveUpdate {
    # label = "FIRMWARE";
    priority = 1;

    type = "0700"; # Microsoft basic data
    attributes = [
      0 # Required Partition
    ];

    size = "1024M";
    content = {
      type = "filesystem";
      format = "vfat";
      # mountpoint = "/boot/firmware";
      mountOptions = [
        "noatime"
        "noauto"
        "x-systemd.automount"
        "x-systemd.idle-timeout=1min"
      ];
    };
  };

  espPartition = lib.recursiveUpdate {
    # label = "ESP";

    type = "EF00"; # EFI System Partition (ESP)
    attributes = [
      2 # Legacy BIOS Bootable, for U-Boot to find extlinux config
    ];

    size = "1024M";
    content = {
      type = "filesystem";
      format = "vfat";
      # mountpoint = "/boot";
      mountOptions = [
        "noatime"
        "noauto"
        "x-systemd.automount"
        "x-systemd.idle-timeout=1min"
        "umask=0077"
      ];
    };
  };
in
{
  imports = [
    inputs.disko.nixosModules.disko
    ./config.txt.nix
  ];
  disko.devices = {
    disk.nvme0n1 = {
      type = "disk";
      device = "/dev/nvme0n1";
      content = {
        type = "gpt";
        partitions = {

          FIRMWARE = firmwarePartition {
            label = "FIRMWARE";
            content.mountpoint = "/boot/firmware";
          };

          ESP = espPartition {
            label = "ESP";
            content.mountpoint = "/boot";
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
  # fileSystems = {
  #   "/boot/firmware" = {
  #     device = "/dev/disk/by-uuid/2175-794E";
  #     fsType = "vfat";
  #     options = [
  #       "noatime"
  #       "noauto"
  #       "x-systemd.automount"
  #       "x-systemd.idle-timeout=1min"
  #     ];
  #   };
  #   # "/" = {
  #   #   device = "/dev/disk/by-uuid/44444444-4444-4444-8888-888888888888";
  #   #   fsType = "ext4";
  #   #   options = [ "noatime" ];
  #   # };
  #   "/" = {
  #     device = "/dev/disk/by-uuid/e769fd8d-1fed-4a59-a987-e21f35294d5f";
  #     fsType = "ext4";
  #     options = [ "noatime" ];
  #   };
  # };
}
