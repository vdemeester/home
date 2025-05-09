{ pkgs, ... }:
{
  boot = {
    loader.systemd-boot.netbootxyz.enable = true;
    # initrd = {
    #   luks.devices."cryptroot" = {
    #     crypttabExtraOpts = [ "fido2-device=auto" ];
    #   };
    #   systemd = {
    #     fido2.enable = true;
    #   };
    # };

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
      # Kernel GPU Savings Options (NOTE i915 chipset only)
      # "i915.enable_rc6=1"
      # "i915.enable_fbc=1"
      # "i915.lvds_use_ssc=0"
      # "drm.debug=0"
      # "drm.vblankoffdelay=1"
      "kvm_intel.nested=1"
      "intel_iommu=on"
    ];

    kernelPackages = pkgs.linuxPackages_latest;
  };
}
