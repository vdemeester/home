{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.dev.js;
in
{
  options = {
    profiles.dev.js = {
      enable = mkOption {
        default = false;
        description = "Enable js development profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      home.file.".npmrc".text = ''
        prefix = ~/.local/npm
      '';
      home.packages = with pkgs; [
        nodejs-10_x
        yarn
      ];
    }
    (mkIf config.profiles.fish.enable {
      xdg.configFile."fish/conf.d/js.fish".text = ''
        set -gx PATH $HOME/.local/npm/bin $PATH
      '';
    })
  ]);
}
