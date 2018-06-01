self: super: {
  
  ape = import ../pkgs/ape { inherit (self) stdenv lib buildGoPackage fetchFromGitHub; };
  clasp = import ../pkgs/clasp { inherit (self) stdenv lib buildGoPackage fetchFromGitHub; };
  tuck = import ../pkgs/tuck { inherit (self) stdenv lib buildGoPackage fetchFromGitHub; };
  dobi = import ../pkgs/dobi { inherit (self) stdenv lib buildGoPackage fetchFromGitHub; };
  envbox = import ../pkgs/envbox { inherit (self) stdenv lib buildGoPackage fetchFromGitHub; };
  prm = import ../pkgs/prm { inherit (self) stdenv lib buildGoPackage fetchgit; };
}
