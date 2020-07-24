{ stdenv }:

stdenv.mkDerivation {
  name = "vde-scripts-0.3";
  builder = ./builder.sh;
  src = ./.;
}
