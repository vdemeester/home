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
      idea = mkOption {
        default = false;
        description = "Install intellij idea";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      profiles.dev.enable = true;
      home.packages = with pkgs; [
        jdk
        gradle
      ];
    }
    (mkIf cfg.idea {
      home.packages = with pkgs; [ jetbrains.idea-ultimate ];
    })
  ]);
}
