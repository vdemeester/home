{ pkgs, ... }:

{
  imports = [
    ./base.nixos.nix
  ];
  profiles.zsh = {
    enable = true;
  };
  profiles.emacs = {
    enable = true;
    texlive = false;
  };
  xdg.configFile."ape.conf".source = ../assets/ape.conf;
}
