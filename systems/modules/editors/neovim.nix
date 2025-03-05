{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.editors.neovim;
in
{
  options.modules.editors.neovim = {
    enable = mkEnableOption "enable neovim editor";
  };
  config = mkIf cfg.enable {
    environment = {
      systemPackages = [ pkgs.neovim ];
    };
  };
}
