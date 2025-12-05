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

  # NFS mounts from rhea
  fileSystems."/net/rhea/music" = {
    device = "rhea.sbr.pm:/music"; # NFSv4: path relative to fsid=0 (/neo)
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

  fileSystems."/net/rhea/pictures" = {
    device = "rhea.sbr.pm:/pictures";
    fsType = "nfs";
    options = [
      "nfsvers=4.2"
      "x-systemd.automount"
      "noauto"
      "x-systemd.idle-timeout=600"
      "soft"
      "timeo=14"
      "retrans=2"
      "_netdev"
    ];
  };

  fileSystems."/net/rhea/videos" = {
    device = "rhea.sbr.pm:/videos";
    fsType = "nfs";
    options = [
      "nfsvers=4.2"
      "x-systemd.automount"
      "noauto"
      "x-systemd.idle-timeout=600"
      "soft"
      "timeo=14"
      "retrans=2"
      "_netdev"
    ];
  };
}
