{ lib, nixosConfig, pkgs, ... }:
let
  inherit (lib) mkIf;
in
{
  home.packages = with pkgs; [ htop ];
  xsession.windowManager.i3 = mkIf nixosConfig.profiles.desktop.enable {
    package = pkgs.i3-gaps;
    enable = true;
  };
}
