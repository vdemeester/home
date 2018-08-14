self: super:
{
  dobi = import ../pkgs/dobi {
    inherit (super) stdenv lib fetchFromGitHub buildGoPackage;
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
