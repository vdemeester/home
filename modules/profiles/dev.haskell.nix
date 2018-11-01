{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.dev.haskell;
in
{
  options = {
    profiles.dev.haskell = {
      enable = mkOption {
        default = false;
        description = "Enable haskell development profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    profiles.dev.enable = true;
    home.packages = with pkgs; [
      ghc
      stack
      hlint
    ];
  };
}
