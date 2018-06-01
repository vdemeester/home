{ pkgs, prefix, ... }:

{
  imports = [ ./git.nix ./emacs.nix ];
  home.packages = with pkgs; [
    gnumake
    cmake
    binutils-unwrapped
    mercurial
    prm
  ];
}
