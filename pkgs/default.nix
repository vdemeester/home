{ system ? builtins.currentSystem }:

let
  pkgs = import <nixpkgs> { inherit system; };
in
rec {
  ape = import ./ape {
    inherit (pkgs) stdenv lib buildGoPackage fetchFromGitHub;
  };
  vrsync = import ./vrsync {
    inherit (pkgs) stdenv lib;
  };
}
