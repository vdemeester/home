{ pkgs, lib, ... }:

{
  imports = [
    ./profiles/bash.nix
    ./profiles/desktop.nix
    ./profiles/ssh.nix
    ./profiles/fish.nix
    ./profiles/i3.nix
    ./profiles/laptop.nix
    ./profiles/tmux.nix
    ./services/shairport-sync.nix
  ];
}
