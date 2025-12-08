{ inputs, lib, ... }:
{
  imports = [
    inputs.disko.nixosModules.disko
    (import ./disks.nix { inherit lib; })

    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-12th-gen

    ../common/hardware/acpid.nix
    ../common/hardware/bluetooth.nix
  ];

  hardware = {
    # opengl.extraPackages = with pkgs; [ vaapiIntel libvdpau-va-gl vaapiVdpau intel-ocl intel-media-driver ];
  };

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
