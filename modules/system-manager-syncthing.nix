# Wrapper module to import nixpkgs syncthing module for system-manager
# This avoids circular dependency by using inputs.nixpkgs instead of pkgs.path
{ inputs, ... }:
{
  imports = [
    "${inputs.nixpkgs}/nixos/modules/services/networking/syncthing.nix"
  ];
}
