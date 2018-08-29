{ stdenv }:

stdenv.mkDerivation {
  name = "emacs-scripts-0.2";
  builder = ./builder.sh;
  src = ./.;
}
