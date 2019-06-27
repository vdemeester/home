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
      autocd = true;
      defaultKeymap = "emacs";
      enableAutosuggestions = true;
      history = {
        expireDuplicatesFirst = true;
        ignoreDups = true;
      };
      shellAliases = import ./aliases.shell.nix;
      initExtra = ''
        if [ -e /home/vincent/.nix-profile/etc/profile.d/nix.sh ]; then . /home/vincent/.nix-profile/etc/profile.d/nix.sh; fi
        autoload -U select-word-style
        select-word-style bash
      '';
      profileExtra = ''
        if [ -e /home/vincent/.nix-profile/etc/profile.d/nix.sh ]; then . /home/vincent/.nix-profile/etc/profile.d/nix.sh; fi
        export NIX_PATH=$HOME/.nix-defexpr/channels:$NIX_PATH
      '';
    };
  };
}
