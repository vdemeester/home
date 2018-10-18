{ pkgs, prefix, ... }:

{
  imports = [ ./git.nix ./emacs.nix ];
  programs.fish.shellAbbrs = {
    m = "make";
  };
  home.packages = with pkgs; [
    binutils-unwrapped
    cmake
    fswatch
    gnumake
    lnav
    mercurial
    ripgrep
    shfmt
  ];
}
