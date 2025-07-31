{
  config,
  lib,
  pkgs,
  ...
}:
{
  options = {
    boot.common-modules = {
      enable = lib.mkEnableOption "Common kernel modules configuration";
      includeNvidia = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Include NVIDIA-related kernel modules";
      };
    };
  };

  config = lib.mkIf config.boot.common-modules.enable {
    boot = {
      initrd.availableKernelModules = [
        "xhci_pci"
        "ahci"
        "nvme"
        "usb_storage"
        "usbhid"
        "sd_mod"
        "sr_mod"
        "dm-mod"
        "thunderbolt"
      ];
      
      kernelModules = [
        "ahci" # sata controller, might not be needed
        "nvme" # required for nvme disks
        "thunderbolt" # required for thunderbolt (dock, …)
        "dm-mod"
        "cryptd" # required for encryption
        "xhci_pci" # usb controller related
        "usb_storage" # usb storage related
        "sd_mod" # block device related
        "sdhci_pci" # block device related as well
        "aesni-intel" # advanced encryption for intel
        "kvm_intel"
        "kvm-intel"
      ];

      kernelPackages = pkgs.linuxPackages_latest;
    };
  };
}