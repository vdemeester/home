{ pkgs, lib, ... }:
{
  imports = [
    ../common/hardware/common-modules.nix
  ];

  # Enable common kernel modules
  boot.common-modules.enable = true;

  environment.systemPackages = with pkgs; [
    sbctl
  ];

  boot = {
    # Secure boot configuration
    bootspec.enable = true;
    # First boot systemd-boot has to be enabled, then switch to lanzaboote
    loader.systemd-boot.enable = lib.mkForce false;
    lanzaboote = {
      enable = true;
      pkiBundle = "/var/lib/sbctl";
    };

    initrd = {
      luks.devices."cryptroot" = {
        crypttabExtraOpts = [ "fido2-device=auto" ];
      };
      systemd = {
        fido2.enable = true;
      };
    };

    # extraModprobeConfig = ''
    #   options snd_hda_intel power_save=1
    # '';

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
