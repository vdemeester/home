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
  kubespy = import ../pkgs/kubespy {
    inherit (self) stdenv lib buildGoPackage fetchgit;
  };
  scripts = import ../pkgs/scripts {
    inherit (self) stdenv;
  };
  envbox = import ../pkgs/envbox {
    inherit (self) stdenv lib buildGoPackage fetchFromGitHub;
  };
  prm = import ../pkgs/prm {
    inherit (self) stdenv lib buildGoPackage fetchgit;
  };
  tmux-tpm = import ../pkgs/tmux-tpm {
    inherit (self) stdenv lib fetchFromGitHub;
  };
}
