{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.gaming;
in
{
  options = {
    profiles.gaming = {
      enable = mkOption {
        default = false;
        description = "Enable gaming profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      steam
      discord
    ];
  };
}
