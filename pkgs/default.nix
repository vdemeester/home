{ system ? builtins.currentSystem }:

let
  pkgs = import <nixpkgs> { inherit system; };
in rec {
  ape = import ./ape {
    inherit (pkgs) stdenv lib buildGoPackage fetchFromGitHub;
  };
  dobi = import ./dobi {
    inherit (pkgs) stdenv lib fetchFromGitHub buildGoPackage;
  };
  scripts = import ./scripts {
    inherit (pkgs) stdenv;
  };
  envbox = import ./envbox {
    inherit (pkgs) stdenv lib buildGoPackage fetchFromGitHub;
  };
  prm = import ./prm {
    inherit (pkgs) stdenv lib fetchgit;
  };
  tmux-tpm = import ./tmux-tpm {
    inherit (pkgs) stdenv lib fetchFromGitHub;
  };

}
