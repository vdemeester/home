{
  config,
  pkgs,
  ...
}:
{
  boot = {
    # supportedFilesystems = [ "zfs" ];
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
      "ahci" # sata controller, might not be needed
      "nvme" # required for nvme disks
      "thunderbolt" # required for thunderbolt (dock, …)
      # from thinkpad x1 gen 9
      "dm-mod"
      "cryptd" # required for encryption
      "xhci_pci" # usb controller related
      "usb_storage" # usb storage related
      "sd_mod" # block device related
      "sdhci_pci" # block device related as well
      "aesni-intel" # advanced encryption for intel
      "kvm_intel"
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
      # ("vfio-pci.ids=" + lib.concatStringsSep "," gpuIDs)
    ];

    kernelPackages = pkgs.linuxPackages_latest;
  };
}
