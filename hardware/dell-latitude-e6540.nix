{ config, pkgs, ... }:

{
  boot.loader.efi.canTouchEfiVariables = true;
  hardware.opengl.extraPackages = [ pkgs.vaapiIntel ];

  boot = {
    kernelParams = [
      # Kernel GPU Savings Options (NOTE i915 chipset only)
      "i915.enable_rc6=0" "i915.enable_fbc=1"
      "i915.lvds_use_ssc=0"
      "drm.debug=0" "drm.vblankoffdelay=1"
    ];
    blacklistedKernelModules = [
      # Kernel GPU Savings Options (NOTE i915 chipset only)
      "sierra_net" "cdc_mbim" "cdc_ncm"
    ];
  };
  services.acpid.enable = true;
}
