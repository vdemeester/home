{ lib, config, ... }:
{
  # Default users, to create everywhere
  imports = [
    ./root.nix
    ./vincent.nix
  ];
  users.motd = with config; ''
    Welcome to ${networking.hostName}

    - This machine is managed by NixOS
    - All changes are futile

    OS:      Nixos ${system.nixos.release} (${system.nixos.codeName}) [${lib.strings.concatStrings system.nixos.tags}]
    Version: ${system.nixos.version}
    Kernel:  ${boot.kernelPackages.kernel.version}
  '';
}
