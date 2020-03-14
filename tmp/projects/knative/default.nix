with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "k8s-dev";
  buildInputs = [
    pkgs.minikube
    pkgs.docker-machine-kvm2
  ];
  shellHook = ''
  echo 'Entering Minikube project environment' 1>&2
  set -v
  minikube version 1>&2
  '';
}
