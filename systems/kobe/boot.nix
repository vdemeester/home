{
  pkgs,
  config,
  ...
}:
{
  boot = {
    loader.systemd-boot.netbootxyz.enable = true;
    # initrd = {
    #   luks.devices."cryptroot" = {
    #     keyFile = "/dev/disk/by-id/mmc-SD08G_0x704a5a38";
    #     keyFileSize = 4096;
    #   };
    # };

    blacklistedKernelModules = [
      "sierra_net" # sierra wireless modules
      "cdc_mbim" # modem mobile broadband modules
      "cdc_ncm" # similar
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
