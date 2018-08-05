{ pkgs, prefix, ... }:

{
  home.packages = with pkgs; [
    mpv
  ];
}
