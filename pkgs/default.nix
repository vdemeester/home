{ system ? builtins.currentSystem }:

let
  pkgs = import <nixpkgs> { inherit system; };
in
rec {
  vrsync = import ./vrsync {
    inherit (pkgs) stdenv lib;
  };
  vde-thinkpad = import ./vde-thinkpad {
    inherit (pkgs) stdenv lib;
  };
}
