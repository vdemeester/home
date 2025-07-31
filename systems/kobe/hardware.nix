{
  inputs,
  lib,
  config,
  ...
}:
{
  imports = [
    inputs.disko.nixosModules.disko
    (import ./disks.nix { inherit lib; })

    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p50

    ../common/hardware/acpid.nix
    ../common/hardware/nvidia.nix
    ../common/hardware/bridge.nix
  ];

  # Enable common configurations
  hardware.nvidia-common.enable = true;
  networking.bridge-common.enable = true;

  networking = {
    # hostId = builtins.substring 0 8 (builtins.hashString "md5" config.networking.hostName); # This was for ZFS
  };

  hardware = {
    enableAllFirmware = true;
  };

}
