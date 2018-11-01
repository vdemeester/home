{ pkgs, lib, ... }:

{
  imports = [
    ./profiles/bash.nix
    ./profiles/desktop.nix
    ./profiles/dev.nix
    ./profiles/emacs.nix
    ./profiles/fish.nix
    ./profiles/git.nix
    ./profiles/i3.nix
    ./profiles/laptop.nix
    ./profiles/ssh.nix
    ./profiles/tmux.nix
    ./services/shairport-sync.nix
  ];
}
