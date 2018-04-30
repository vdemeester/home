{ pkgs, prefix, ... }:

{
  imports = [ ./git.nix ];
  home.packages = with pkgs; [
    gnumake
    mercurial
  ];
}
