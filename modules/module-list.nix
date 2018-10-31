{ pkgs, lib, ... }:

{
  imports = [
    ./profiles/i3.nix
    ./services/shairport-sync.nix
  ];
}
