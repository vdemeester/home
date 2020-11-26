{ sources ? import ./nix
, lib ? sources.lib
, pkgs ? sources.pkgs { }
, pkgs-unstable ? sources.pkgs-unstable { }
, nixpkgs ? sources.nixpkgs { }
}@args:
let
  foo = "bar";
in
{
  tools.bus = pkgs.callPackage ./tools/bus { };
  tools.univ = pkgs.callPackage ./tools/univ { };
  tools.system = pkgs.callPackage ./tools/system { };
}
