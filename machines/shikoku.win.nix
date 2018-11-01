{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];
  profiles.dev.go.enable = true;
  home.packages = with pkgs; [ docker ];
}
