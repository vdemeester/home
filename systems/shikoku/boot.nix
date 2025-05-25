{
  config,
  lib,
  ...
}:
let
  gpuIDs = [
    "10de:1b80" # Graphics
    "10de:10f0" # Audio
  ];
in
{
  boot = {
    supportedFilesystems = [ "zfs" ];
    initrd.availableKernelModules = [
      "xhci_pci"
      "ahci"
      "nvme"
      "usb_storage"
      "usbhid"
      "sd_mod"
      "sr_mod"
    ];
    initrd.kernelModules = [
      "vfio_pci"
      "vfio"
      "vfio_iommu_type1"

      "nvidia"
      "nvidia_modeset"
      "nvidia_uvm"
      "nvidia_drm"
    ];
    kernelModules = [
      "kvm-intel"
      "nvidia"
    ];
    extraModulePackages = [
      config.boot.kernelPackages.nvidiaPackages.stable
      config.boot.kernelPackages.nvidia_x11
    ];
    kernelParams = [
      "intel_iommu=on"
      "kvm_intel.nested=1"
      ("vfio-pci.ids=" + lib.concatStringsSep "," gpuIDs)
    ];
    boot.initrd.kernelModules = [ "nvidia" ];
  };
}
