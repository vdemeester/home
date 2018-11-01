{ pkgs, lib, ... }:

{
  imports = [
    ./profiles/bash.nix
    ./profiles/desktop.nix
    ./profiles/ssh.nix
    ./profiles/fish.nix
    ./profiles/i3.nix
    ./services/shairport-sync.nix
  ];
}
