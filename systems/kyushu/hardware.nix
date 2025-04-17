{ inputs, lib, ... }:
{
  imports = [
    inputs.disko.nixosModules.disko
    (import ./disks.nix { inherit lib; })

    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-12th-gen

    ../common/hardware/acpid.nix
    ../common/hardware/bluetooth.nix
  ];

  hardware = {
    # opengl.extraPackages = with pkgs; [ vaapiIntel libvdpau-va-gl vaapiVdpau intel-ocl intel-media-driver ];
  };
}
