{ lib, ... }:
{
  boot.loader.grub.device = "/dev/vda";
  boot.loader.grub.enable = lib.mkForce true;
  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.initrd.systemd.enable = lib.mkForce false;

  ## From qemu-quest.nix
  boot.initrd.availableKernelModules = [
    "virtio_net"
    "virtio_pci"
    "virtio_mmio"
    "virtio_blk"
    "virtio_scsi"
    "9p"
    "9pnet_virtio"
  ];
  boot.initrd.kernelModules = [
    "virtio_balloon"
    "virtio_console"
    "virtio_rng"
  ];

  boot.initrd.postDeviceCommands = ''
    # Set the system time from the hardware clock to work around a
    # bug in qemu-kvm > 1.5.2 (where the VM clock is initialised
    # to the *boot time* of the host).
    hwclock -s
  '';

}
