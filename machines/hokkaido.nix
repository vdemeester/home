{ pkgs, ... }:

{
  imports = [
    ./base.fedora.nix
  ];
  home.packages = with pkgs; [
    kubectx
    kustomize
  ];
  profiles.finances.enable = true;
  profiles.zsh = {
    enable = true;
  };
  profiles.dev = {
    enable = true;
  };
  profiles.emacs = {
    enable = true;
    texlive = false;
    daemonService = false;
  };
  xdg.configFile."user-dirs.dirs".source = ../modules/profiles/assets/xorg/user-dirs.dirs;
}
