{ pkgs, prefix, ... }:

{
  home.packages = with pkgs; [
    gnumake
  ];
}
