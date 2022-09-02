{ pkgs ? import <nixpkgs> { }, ... }:
let
  go = pkgs.go_1_18;
in
pkgs.mkShell {
  name = "tektoncd";
  buildInputs = with pkgs; [
    go
    ko
    my.oc
    my.tkn
    google-cloud-sdk
    gron
  ];
  shellHook = ''
    export GOMODULE=on
    export GOFLAGS="-mod=vendor"
    export GOROOT=${go}/share/go
    export GOMAXPROCS=8
    export KUSTOMIZE_BIN=${pkgs.kustomize}/bin/kustomize
    export KO_BIN=${pkgs.ko}/bin/ko
    export KO_DOCKER_REPO=gcr.io/vde-tekton
  '';
}
