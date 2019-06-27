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
      initExtra = ''
        if [ -e /home/vincent/.nix-profile/etc/profile.d/nix.sh ]; then . /home/vincent/.nix-profile/etc/profile.d/nix.sh; fi
      '';
      profileExtra = ''
        if [ -e /home/vincent/.nix-profile/etc/profile.d/nix.sh ]; then . /home/vincent/.nix-profile/etc/profile.d/nix.sh; fi
        export NIX_PATH=$HOME/.nix-defexpr/channels:$NIX_PATH
      '';
    };
  };
}
