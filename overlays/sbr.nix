self: super:
{
  ape = import ../pkgs/ape {
    inherit (super) stdenv lib buildGoPackage fetchFromGitHub;
  };
  dobi = import ../pkgs/dobi {
    inherit (super) stdenv lib buildGoPackage fetchFromGitHub;
  };
  emacs-scripts = import ../pkgs/emacs-scripts {
    inherit (super) stdenv;
  };
  envbox = import ../pkgs/envbox {
    inherit (super) stdenv lib buildGoPackage fetchFromGitHub;
  };
  op = import ../pkgs/op {
    inherit (super) stdenv lib fetchurl patchelf unzip file;
  };
  prm = import ../pkgs/prm {
    inherit (super) stdenv lib buildGoPackage fetchgit;
  };
  tmux-tpm = import ../pkgs/tmux-tpm {
    inherit (super) stdenv lib fetchFromGitHub;
  };
}
