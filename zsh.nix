{ pkgs, config, lib, ...}:

{
  programs.zsh = {
    enable = true;
    shellAliases = import ./aliases.nix
  };
}
