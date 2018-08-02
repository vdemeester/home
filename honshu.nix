{ pkgs, prefix, ... }:

{
  imports = [
    ./devops.nix
    ./dev.go.nix
    ./dev.rust.nix
    ./dev.python.nix
    ./dev.js.nix
    ./dev.haskell.nix
  ];
}
