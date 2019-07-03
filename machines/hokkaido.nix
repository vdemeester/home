{ pkgs, ... }:

{
  imports = [
    ./base.nixos.nix
  ];
  profiles.zsh = {
    enable = true;
  };
  xdg.configFile."ape.conf".source = ../assets/ape.conf;
}
