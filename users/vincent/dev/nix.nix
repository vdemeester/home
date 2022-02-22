{ pkgs, ... }:

{
  home.packages = with pkgs; [
    niv
    nixpkgs-fmt
    nix-update
    nix-review
    nix-prefetch-scripts
  ];
}
