{
  inputs,
  ...
}:
{
  imports = [
    # inputs.disko.nixosModules.diskop # Do this if re-install
    # (import ./disks.nix { inherit lib; })

    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3

    ../common/hardware/acpid.nix
    # ../common/hardware/bluetooth.nix
    ../common/services/nfs-mounts.nix
  ];

  hardware = {
    enableAllFirmware = true;
  };

  # FILESYSTEM
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/91b05f64-b97d-4405-8405-8785699ada8f";
      preLVM = true;
      allowDiscards = true;
      keyFile = "/dev/disk/by-id/mmc-SD08G_0x704a5a38";
      keyFileSize = 4096;
    };
  };

  fileSystems."/" = {
    # device = "/dev/disk/by-uuid/6bedd234-3179-46f7-9a3f-feeffd880791";
    device = "/dev/mapper/root";
    fsType = "ext4";
    options = [
      "noatime"
      "discard"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/32B9-94CC";
    fsType = "vfat";
  };

  swapDevices = [ { device = "/dev/disk/by-uuid/24da6a46-cd28-4bff-9220-6f449e3bd8b5"; } ];

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
