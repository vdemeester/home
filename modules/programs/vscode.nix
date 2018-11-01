{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.vscode;
in
{
  options = {
    programs.vscode = {
      enable = mkOption {
        default = false;
        description = "Enable VS Code profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      vscode-with-extensions
    ];
  };
}
