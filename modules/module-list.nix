{ pkgs, lib, ... }:

{
  imports = [
    ./services/shairport-sync.nix
  ];
}
