{
  pkgs,
  config,
  lib,
  ...
}:
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
    # bootspec.enable = true;
    # First boot systemd-boot has to be enabled, then switch to lanzaboote
    # loader.systemd-boot.enable = lib.mkForce false;
    # lanzaboote = {
    # enable = true;
    # pkiBundle = "/var/lib/sbctl";
    # };
    initrd.availableKernelModules = [
      "rtsx_pci_sdmmc"
    ];
    # loader.systemd-boot.netbootxyz.enable = true;
    initrd.luks.devices."cryptroot" = {
      keyFile = "/dev/disk/by-id/mmc-SDC_0x00011fd6";
      keyFileSize = 4096;
    };
    initrd.systemd.enableTpm2 = lib.mkForce false;

    blacklistedKernelModules = [
      "sierra_net" # sierra wireless modules
      "cdc_mbim" # modem mobile broadband modules
      "cdc_ncm" # similar
    ];
  };
}
