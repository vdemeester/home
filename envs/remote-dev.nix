{...}:

{
  imports = [
    ./fish.nix
    ./base.nix
    ./dev.go.nix
    ./dev.js.nix
    ./dev.rust.nix
  ];
  manual.manpages.enable = false;
}
