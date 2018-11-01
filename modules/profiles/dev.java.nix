{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.dev.java;
in
{
  options = {
    profiles.dev.java = {
      enable = mkOption {
        default = false;
        description = "Enable java development profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    profiles.dev.enable = true;
    home.packages = with pkgs; [
      jdk
      gradle
    ];
  };
}
