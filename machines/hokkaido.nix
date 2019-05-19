{ pkgs, ... }:

{
  imports = [
    ./base.fedora.nix
  ];
  xdg.configFile."user-dirs.dirs".source = ../modules/profiles/assets/xorg/user-dirs.dirs;
  profiles.finances.enable = true;
  profiles.dev = {
    enable = true;
  };
  profiles.emacs = {
    enable = true;
    texlive = false;
    daemonService = false;
  };
}
