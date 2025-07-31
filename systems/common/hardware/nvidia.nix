{
  config,
  lib,
  pkgs,
  ...
}:
{
  options = {
    hardware.nvidia-common = {
      enable = lib.mkEnableOption "NVIDIA common configuration";
    };
  };

  config = lib.mkIf config.hardware.nvidia-common.enable {
    hardware.nvidia = {
      modesetting.enable = true;
      open = false;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };
    hardware.graphics = {
      enable = true;
    };
    nixpkgs.config.allowUnfree = true;

    boot = {
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
        "nvidia"
      ];
      extraModulePackages = [
        config.boot.kernelPackages.nvidiaPackages.stable
        config.boot.kernelPackages.nvidia_x11
      ];
      kernelParams = [
        "intel_iommu=on"
        "kvm_intel.nested=1"
      ];
    };
  };
}