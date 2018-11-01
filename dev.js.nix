{ pkgs, prefix, ... }:

{
  profiles.dev.enable = true;
  home.file.".npmrc".text = ''
    prefix = ~/.local/npm
  '';
  xdg.configFile."fish/conf.d/js.fish".text = ''
    set -gx PATH $HOME/.local/npm/bin $PATH
  '';
  home.packages = with pkgs; [
    nodejs-10_x
    yarn
  ];
}
