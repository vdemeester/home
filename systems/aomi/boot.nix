{ pkgs, ... }:
{
  imports = [
    ../common/hardware/common-modules.nix
  ];

  # Enable common kernel modules
  boot.common-modules.enable = true;

  boot = {
    loader.systemd-boot.netbootxyz.enable = true;
    # initrd.systemd.enable = lib.mkForce false;
    initrd.availableKernelModules = [
      "rtsx_pci_sdmmc"
    ];
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

    kernelParams = [
      # Kernel GPU Savings Options (NOTE i915 chipset only)
      # "i915.enable_rc6=1"
      # "i915.enable_fbc=1"
      # "i915.lvds_use_ssc=0"
      # "drm.debug=0"
      # "drm.vblankoffdelay=1"
    ];
  };
}
