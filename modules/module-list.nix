{ pkgs, lib, ... }:

{
  imports = [
    ./profiles/i3.nix
    ./profiles/desktop.nix
    ./services/shairport-sync.nix
  ];
}
