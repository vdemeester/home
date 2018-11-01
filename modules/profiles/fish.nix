{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.fish;
in
{
  options = {
    profiles.fish = {
      enable = mkOption {
        default = true;
        description = "Enable fish program and configurations";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    programs.fish = {
    enable = true;
    shellAliases = import ./aliases.shell.nix;
    shellInit = ''
      eval (${pkgs.direnv}/bin/direnv hook fish)
      # emacs ansi-term support
      if test -n "$EMACS"
        set -x TERM eterm-color
  
        # this function may be required
        function fish_title
          true
        end
      end
      '';
    };
    xdg.configFile."fish/conf.d/a_nix_run.fish".source = ./assets/fish/a_nix_run.fish;
    xdg.configFile."fish/conf.d/nix-aliases.fish".source = ./assets/fish/nix-aliases.fish;
    xdg.configFile."fish/conf.d/sudope.fish".source = ./assets/fish/sudope.fish;
    xdg.configFile."fish/functions/sudope.fish".source = ./assets/fish/sudope.function.fish;
    xdg.configFile."fish/functions/fish_prompt.fish".source = ./assets/fish/fish_prompt.fish;
    xdg.configFile."fish/functions/fish_right_prompt.fish".source = ./assets/fish/fish_right_prompt.fish;
  };
}
