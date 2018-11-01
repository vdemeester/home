{ pkgs, prefix, ... }:

{
  imports = [
    ./base.nix
  ];
  profiles.dev.go.enable = true;
  profiles.media.enable = true;
  xdg.configFile."ape.conf".source = ./assets/ape.conf;
}
