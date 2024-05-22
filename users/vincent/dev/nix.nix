{ pkgs, ... }:

{
  home.packages = with pkgs; [
    nix-output-monitor
    nix-prefetch-scripts
    nix-update
    nixpkgs-fmt
    nixpkgs-review
    nurl
  ];
}
