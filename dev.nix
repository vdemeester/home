{ pkgs, prefix, ... }:

{
  imports = [ ./git.nix ./emacs.nix ];
  programs.fish.shellAbbrs = {
    m = "make";
  };
  home.packages = with pkgs; [
    gnumake
    cmake
    binutils-unwrapped
    mercurial
    ripgrep
  ];
}
