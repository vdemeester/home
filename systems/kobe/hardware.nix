{ inputs, lib, ... }:
{
  imports = [
    inputs.disko.nixosModules.disko
    (import ./disks.nix { inherit lib; })

    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p50

  ];

}
