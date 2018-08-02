{ pkgs, config, lib, ...}:

{
  programs.bash = {
    enable = true;
    shellAliases = import ./aliases.nix
  };
}
