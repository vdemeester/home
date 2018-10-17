{ pkgs, prefix, ... }:

{
  imports = [ ./git.nix ./emacs.nix ];
  programs.fish.shellAbbrs = {
    m = "make";
  };
  home.packages = with pkgs; [
    binutils-unwrapped
    cmake
    gnumake
    lnav
    mercurial
    ripgrep
    shfmt
  ];
}
