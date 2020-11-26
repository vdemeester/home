{ stdenv }:

stdenv.mkDerivation {
  name = "system";
  src = ./.;
  phases = [ "installPhase" "fixupPhase" ];
  installPhase = ''
    mkdir -p $out $out/bin
    cp $src/system $out/bin/system
  '';
}
