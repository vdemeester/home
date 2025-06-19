{
  pkgs ? import <nixpkgs> {
    overlays = [
      (_self: _super: { })
    ];
  },
  ...
}:
let
  go = pkgs.go_1_18;
in
pkgs.mkShell {
  name = "osp";
  buildInputs = with pkgs; [
    curl
    docker-client
    gawk
    git-crypt
    gnumake
    gron
    jq
    kustomize
    oc
    operator-sdk
    operator-tool
    tektoncd-cli
    opm
    python39Packages.pyaml
    skopeo
    yq-go
    yamllint
    go
  ];
  shellHook = ''
    export GOMODULE=on
    export GOFLAGS="-mod=vendor"
    export GOROOT=${go}/share/go
    export GOMAXPROCS=8
    export KUSTOMIZE_BIN=${pkgs.kustomize}/bin/kustomize
  '';
}
