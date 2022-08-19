{ config, pkgs, ... }:
{
  # NixOS options
  boot = {
    blacklistedKernelModules = [
      "sierra_net" # sierra wireless modules
      "cdc_mbim" # modem mobile broadband modules
      "cdc_ncm" # similar
    ];
    extraModprobeConfig = ''
      options snd_hda_intel power_save=1
    '';
    initrd = {
      availableKernelModules = [
        "nvme" # required for nvme disks
        "thunderbolt" # required for thunderbolt (dock, ...)
        "dm-mod"
        "cryptd" # required for encryption
        "ahci" # sata controller, might not be needed
        "xhci_pci" # usb controller related
        "usb_storage" # usb storage related
        "sd_mod" # block device related
        "sdhci_pci" # block device related as well
        "aesni-intel" # advanced encryption for intel
      ];
    };
    loader.efi.canTouchEfiVariables = true;
  };
  hardware = {
    trackpoint.enable = false;
    cpu.intel.updateMicrocode = true;
  };
}
