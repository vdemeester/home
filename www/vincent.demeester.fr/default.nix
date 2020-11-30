{ stdenv }:

stdenv.mkDerivation {
  name = "vincent.demeester.fr";
  src = ./.;
}
