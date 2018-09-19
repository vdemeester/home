{ pkgs, prefix, ... }:

{
  imports = [ ./dev.nix ];
  home.packages = with pkgs; [
    ghc
    stack
    hlint
  ];
}
