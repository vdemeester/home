{ config, lib, ... }:

with lib;
let
  cfg = config.profiles.zsh;
in
{
  options = {
    profiles.zsh = {
      enable = mkOption {
        default = false;
        description = "Enable zsh profile and configuration";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      shellAliases = import ./aliases.shell.nix;
    };
  };
}
