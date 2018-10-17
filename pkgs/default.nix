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
  openshift = import ./openshift {
    inherit (pkgs) stdenv lib fetchFromGitHub removeReferencesTo which go_1_10 go-bindata makeWrapper rsync utillinux coreutils kerberos clang;
  };
  kubespy = import ./kubespy {
    inherit (pkgs) stdenv lib buildGoPackage fetchgit;
  };
  knctl = import ./knctl {
    inherit (pkgs) stdenv lib buildGoPackage fetchFromGitHub;
  };
  krew = import ./krew {
    inherit (pkgs) stdenv lib buildGoPackage fetchFromGitHub;
  };
  kube-prompt = import ./kube-prompt {
    inherit (pkgs) stdenv lib buildGoPackage fetchFromGitHub;
  };
  scripts = import ./scripts {
    inherit (pkgs) stdenv;
  };
  skaffold = import ./skaffold {
    inherit (pkgs) stdenv lib buildGoPackage fetchFromGitHub;
  };
  s2i = import ./s2i {
    inherit (pkgs) stdenv lib buildGoPackage fetchFromGitHub;
  };
  envbox = import ./envbox {
    inherit (pkgs) stdenv lib buildGoPackage fetchFromGitHub;
  };
  prm = import ./prm {
    inherit (pkgs) stdenv lib buildGoPackage fetchFromGitHub;
  };
  tmux-tpm = import ./tmux-tpm {
    inherit (pkgs) stdenv lib fetchFromGitHub;
  };

}
