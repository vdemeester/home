{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.dev.rust;
in
{
  options = {
    profiles.dev.rust = {
      enable = mkOption {
        default = false;
        description = "Enable rust development profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      profiles.dev.enable = true;
      home.packages = with pkgs; [
        rustup
      ];
    }
    (mkIf config.profiles.emacs.enable {
      home.packages = with pkgs; [
        rustracer
      ];
    })
  ]);
}
