{ inputs, lib, ... }:
{
  imports = [
    inputs.disko.nixosModules.disko
    (import ./disks.nix { inherit lib; })

    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-12th-gen

    ../common/hardware/acpid.nix
    ../common/hardware/bluetooth.nix
    ../common/services/nfs-rhea-mounts.nix
  ];

  hardware = {
    # opengl.extraPackages = with pkgs; [ vaapiIntel libvdpau-va-gl vaapiVdpau intel-ocl intel-media-driver ];
  };

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

  # NFS mounts from synodine (NFSv3)
  fileSystems."/net/synodine/usbshare" = {
    device = "synodine.home:/volumeUSB2/usbshare";
    fsType = "nfs";
    options = [
      "nfsvers=3"
      "x-systemd.automount"
      "noauto"
      "x-systemd.idle-timeout=600"
      "soft"
      "_netdev"
    ];
  };

  fileSystems."/net/synodine/downloads" = {
    device = "synodine.home:/volume1/downloads";
    fsType = "nfs";
    options = [
      "nfsvers=3"
      "x-systemd.automount"
      "noauto"
      "x-systemd.idle-timeout=600"
      "soft"
      "_netdev"
    ];
  };

  fileSystems."/net/synodine/video" = {
    device = "synodine.home:/volume1/video";
    fsType = "nfs";
    options = [
      "nfsvers=3"
      "x-systemd.automount"
      "noauto"
      "x-systemd.idle-timeout=600"
      "soft"
      "_netdev"
    ];
  };
}
