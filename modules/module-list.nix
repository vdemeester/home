{ pkgs, lib, ... }:

{
  imports = [
    ./profiles/bash.nix
    ./profiles/desktop.nix
    ./profiles/dev.nix
    ./profiles/dev.go.nix
    ./profiles/dev.haskell.nix
    ./profiles/dev.java.nix
    ./profiles/dev.js.nix
    ./profiles/dev.python.nix
    ./profiles/dev.rust.nix
    ./profiles/emacs.nix
    ./profiles/fish.nix
    ./profiles/git.nix
    ./profiles/i3.nix
    ./profiles/laptop.nix
    ./profiles/ssh.nix
    ./profiles/tmux.nix
    ./profiles/zsh.nix
    ./programs/vscode.nix
    ./services/shairport-sync.nix
  ];
}
