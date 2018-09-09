self: super: {
  ape = import ../pkgs/ape { inherit (self) stdenv lib buildGoPackage fetchFromGitHub; };
  vrsync = import ../pkgs/vrsync { inherit (self) stdenv lib; };
}
