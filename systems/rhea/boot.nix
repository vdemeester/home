{ lib, pkgs, ... }:
{
  boot = {
    initrd = {
      availableKernelModules = [
        "nvme"
        "ahci"
        "usbhid"
      ];
      kernelModules = [ ];
    };
    kernelModules = [ ];
    extraModulePackages = [ ];
    kernelPackages = pkgs.linuxPackages_latest;
    loader = {
      efi.canTouchEfiVariables = true;
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };
  };
}
