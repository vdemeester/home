self: super:
{
  ape = import ../pkgs/ape {
    inherit (super) stdenv lib buildGoPackage fetchFromGitHub;
  };
  dobi = import ../pkgs/dobi {
    inherit (super) stdenv lib buildGoPackage fetchFromGitHub;
  };
  scripts = import ../pkgs/scripts {
    inherit (super) stdenv;
  };
  envbox = import ../pkgs/envbox {
    inherit (super) stdenv lib buildGoPackage fetchFromGitHub;
  };
  prm = import ../pkgs/prm {
    inherit (super) stdenv lib buildGoPackage fetchgit;
  };
  tmux-tpm = import ../pkgs/tmux-tpm {
    inherit (super) stdenv lib fetchFromGitHub;
  };
}
