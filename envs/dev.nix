{ pkgs, prefix, ... }:

{
  imports = [ ./git.nix ./emacs.nix ];
  home.packages = with pkgs; [
    gnumake
    mercurial
  ];
}
