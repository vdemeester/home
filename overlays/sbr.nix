self: super:
{
  prm = import ../pkgs/prm {
      inherit (super) stdenv lib buildGoPackage fetchgit;
  };
  tmux-tpm = import ../pkgs/tmux-tpm {
    inherit (super) stdenv lib fetchFromGitHub;
  };
}
