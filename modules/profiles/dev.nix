{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.dev;
in
{
  options = {
    profiles.dev = {
      enable = mkEnableOption "Enable development profile";
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
        mercurial
        niv
        ripgrep
        shfmt
      ];
      xdg.configFile."nr/dev" = {
        text = builtins.toJSON [
          {cmd = "yq";} {cmd = "lnav";} {cmd = "miniserve";}
          { cmd = "licensor"; } { cmd = "nix-review"; }
          {cmd = "yamllint"; pkg = "python37Packages.yamllint";}
          {cmd = "nix-prefetch-git"; pkg = "nix-prefetch-scripts";}
          {cmd = "nix-prefetch-hg"; pkg = "nix-prefetch-scripts";}
          {cmd = "http"; pkg = "httpie"; }
        ];
        onChange = "${pkgs.nur.repos.vdemeester.nr}/bin/nr dev";
      };
      services.lorri.enable = true;
    }
  ]);
}
