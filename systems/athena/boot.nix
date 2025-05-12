{ lib, pkgs, ... }:
{
  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_rpi4;
    initrd.availableKernelModules = [
      "xhci_pci"
      "usbhid"
      "usb_storage"
    ];
    loader = {
      grub.enable = false;
      systemd-boot.enable = lib.mkForce false;
      generic-extlinux-compatible.enable = true;
    };
  };
}
