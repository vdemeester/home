{ stdenv }:

stdenv.mkDerivation {
  name = "vde-scripts-0.1";
  builder = ./builder.sh;
  src = ./.;
}
