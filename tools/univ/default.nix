{ stdenv }:

stdenv.mkDerivation {
  name = "univ";
  src = ./.;
  phases = [ "installPhase" "fixupPhase" ];
  installPhase = ''
    mkdir -p $out $out/bin
    cp $src/univ.sh $out/bin/univ
  '';
}
