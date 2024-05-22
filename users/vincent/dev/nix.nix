{ pkgs, ... }:

{
  home.packages = with pkgs; [
    niv
    nixpkgs-fmt
    nix-update
    nixpkgs-review
    nix-prefetch-scripts
    nurl
    # rnix-lsp
  ];
}
