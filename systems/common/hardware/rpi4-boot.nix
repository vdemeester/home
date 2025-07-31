{ lib, pkgs, ... }:
{
  console.keyMap = lib.mkForce "us";
  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_rpi4;
    initrd.systemd.enable = lib.mkForce false;
    # initrd.systemd.enableTpm2 = false;
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