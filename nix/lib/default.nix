inputs:

# personal lib
let
  inherit (inputs.nixpkgs) lib;

  home = import ./home.nix inputs;
  system = import ./system.nix inputs;
in
lib // home // system
