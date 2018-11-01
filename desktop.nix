{ pkgs, prefix, ... }:

{
  imports = [ ./base.nix ];
  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.fr
    keybase
    peco
    spotify
    youtube-dl
  ];
}
