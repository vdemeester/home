{ pkgs, lib, ... }:

{
  imports = [
    ./programs/podman.nix
  ];
}
