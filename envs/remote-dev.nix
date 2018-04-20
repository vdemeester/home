{ pkgs, prefix, ... }:

{
  imports = [
    ./common.nix
    ./laptop.nix
    ./dev.nix
  ];
}
