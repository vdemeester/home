{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.dev;
in
{
  options = {
    profiles.dev = {
      enable = mkOption {
        default = false;
        description = "Enable development profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      profiles.git.enable = true;
      profiles.emacs.enable = true;
      home.file.".ignore".text = ''
      *.swp
      *~
      **/VENDOR-LICENSE
      '';
      home.packages = with pkgs; [
        binutils
        cmake
        fswatch
        gnumake
        jq
        lnav
        mercurial
        ripgrep
        shfmt
        yq
      ];
    }
    (mkIf config.profiles.fish.enable {
      programs.fish.shellAbbrs = {
        m = "make";
      };
    })
  ]);
}
