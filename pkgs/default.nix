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
  dep-collector = import ./dep-collector {
    inherit (pkgs) stdenv lib fetchgit buildGoPackage;
  };
  protobuild = import ./protobuild {
    inherit (pkgs) stdenv lib buildGoPackage fetchgit;
  };
  go-containerregistry = import ./go-containerregistry {
    inherit (pkgs) stdenv lib buildGoPackage fetchgit;
  };
  gogo-protobuf = import ./gogo-protobuf {
    inherit (pkgs) stdenv lib buildGoPackage fetchgit;
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
