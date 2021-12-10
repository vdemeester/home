let
  sources = import /etc/nixos/nix;
  # pkgs = sources.nixpkgs { };
  pkgs = sources.pkgs-unstable { };
  my = import /etc/nixos/nix/packages { pkgs = pkgs; };
  go = pkgs.go_1_16;
in
pkgs.mkShell {
  name = "osp";
  buildInputs = with pkgs; [
    go
    gron
    yq-go
    jq
    python39Packages.pyaml
    my.oc
    my.tkn
    my.operator-sdk
  ];
  shellHook = ''
    export GOMODULE=on
    export GOFLAGS="-mod=vendor"
    export GOROOT=${go}/share/go
    export GOMAXPROCS=8
    export KUSTOMIZE_BIN=${pkgs.kustomize}/bin/kustomize
  '';
}
