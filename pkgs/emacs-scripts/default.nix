{ stdenv }:

stdenv.mkDerivation {
  name = "emacs-scripts-0.1";
  builder = ./builder.sh;
  src = ./.;
}
