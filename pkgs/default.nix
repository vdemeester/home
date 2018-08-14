{ system ? builtins.currentSystem }:

let
  pkgs = import <nixpkgs> { inherit system; };
in rec {
  dobi = import ./dobi {
    inherit (pkgs) stdenv lib fetchFromGitHub buildGoPackage;
  };
  op = import ./op {
    inherit (pkgs) stdenv lib fetchurl patchelf unzip file;
  };
  prm = import ./prm {
    inherit (pkgs) stdenv lib fetchgit;
  };
  tmux-tpm = import ./tmux-tpm {
    inherit (pkgs) stdenv lib fetchFromGitHub;
  };

}
