{ lib, nixosConfig, pkgs, ... }:
let
  inherit (lib) mkIf;
in
{
  profiles = {
    desktop = {
      i3.enable = true;
      browsers = {
        default = "firefox";
        firefox.enable = true;
      };
      term = {
        default = "alacritty";
        alacritty.enable = true;
      };
    };
    editors = {
      default = "emacs";
      emacs.enable = true;
      vim.enable = true;
      # vscode.enable = false;
    };
    dev = {
      go.enable = true;
      python.enable = true;
    };
    hardware = {
      ergodox.enable = true;
    };
    shell = {
      git.enable = true;
      gnupg.enable = true;
      direnv.enable = true;
      tmux.enable = true;
      zsh.enable = true;
    };
  };
  home.packages = with pkgs; [ htop ];
  xsession.windowManager.i3 = mkIf nixosConfig.profiles.desktop.enable {
    package = pkgs.i3-gaps;
    enable = true;
  };
}
