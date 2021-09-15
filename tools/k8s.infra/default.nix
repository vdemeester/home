{ stdenv }:

stdenv.mkDerivation {
  name = "k8s.infra";
  src = ./.;
  phases = [ "installPhase" "fixupPhase" ];
  installPhase = ''
    mkdir -p $out $out/bin
    cp $src/k8s.infra.sh $out/bin/k8s.infra
  '';
}
