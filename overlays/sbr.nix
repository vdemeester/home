self: super:
{
  ape = import ../pkgs/ape {
    inherit (self) stdenv lib buildGoPackage fetchFromGitHub;
  };
  dobi = import ../pkgs/dobi {
    inherit (self) stdenv lib buildGoPackage fetchFromGitHub;
  };
  dep-collector = import ../pkgs/dep-collector {
    inherit (self) stdenv lib fetchgit buildGoPackage;
  };
  protobuild = import ../pkgs/protobuild {
    inherit (self) stdenv lib buildGoPackage fetchgit;
  };
  go-containerregistry = import ../pkgs/go-containerregistry {
    inherit (self) stdenv lib buildGoPackage fetchgit;
  };
  gogo-protobuf = import ../pkgs/gogo-protobuf {
    inherit (self) stdenv lib buildGoPackage fetchgit;
  };
  knctl = import ../pkgs/knctl {
    inherit (self) stdenv lib buildGoPackage fetchFromGitHub;
  };
  krew = import ../pkgs/krew {
    inherit (self) stdenv lib buildGoPackage fetchFromGitHub;
  };
  kubespy = import ../pkgs/kubespy {
    inherit (self) stdenv lib buildGoPackage fetchgit;
  };
  kube-prompt = import ../pkgs/kube-prompt {
    inherit (self) stdenv lib buildGoPackage fetchFromGitHub;
  };
  scripts = import ../pkgs/scripts {
    inherit (self) stdenv;
  };
  s2i= import ../pkgs/s2i {
    inherit (self) stdenv lib buildGoPackage fetchFromGitHub;
  };
  slirp4netns = import ../pkgs/slirp4netns {
    inherit (self) stdenv lib fetchFromGitHub automake autoconf gcc;
  };
  envbox = import ../pkgs/envbox {
    inherit (self) stdenv lib buildGoPackage fetchFromGitHub;
  };
  prm = import ../pkgs/prm {
    inherit (self) stdenv lib buildGoPackage fetchFromGitHub;
  };
  tmux-tpm = import ../pkgs/tmux-tpm {
    inherit (self) stdenv lib fetchFromGitHub;
  };
  vscodeliveshare = import ../pkgs/vscodeliveshare {
    inherit (self) stdenv vscode-utils autoPatchelfHook xorg gnome3 utillinux openssl icu zlib curl lttng-ust libsecret libkrb5 gcc libunwind binutils;
  };
}
