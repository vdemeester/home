{...}:

{
  imports = [
    ./base.nix
    ./dev.js.nix
    ./dev.rust.nix
  ];
  profiles.dev.go.enable = true;
  manual.manpages.enable = false;
}
