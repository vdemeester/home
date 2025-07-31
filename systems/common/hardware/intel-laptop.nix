{ pkgs, ... }:
{
  boot = {
    loader.systemd-boot.netbootxyz.enable = true;
    initrd.availableKernelModules = [
      "nvme"
      "rtsx_pci_sdmmc"
      "thunderbolt"
      "dm-mod"
    ];

    blacklistedKernelModules = [
      "sierra_net" # sierra wireless modules
      "cdc_mbim" # modem mobile broadband modules
      "cdc_ncm" # similar
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
    ];

    kernelParams = [
      "kvm_intel.nested=1"
      "intel_iommu=on"
    ];

    kernelPackages = pkgs.linuxPackages_latest;
    loader.efi.canTouchEfiVariables = true;
  };
  hardware = {
    cpu.intel.updateMicrocode = true;
  };
}