let
  sources = import /etc/nixos/nix;
  # pkgs = sources.nixpkgs { };
  pkgs = sources.pkgs-unstable { };
  my = import /etc/nixos/nix/packages { pkgs = pkgs; };
in
pkgs.mkShell {
  name = "tektoncd";
  buildInputs = with pkgs; [
    my.ko
    my.oc
    my.tkn
    google-cloud-sdk
    gron
  ];
  shellHook = ''
    export GOMODULE=on
    export GOFLAGS="-mod=vendor"
    export GOROOT=${pkgs.go}/share/go
    export KUSTOMIZE_BIN=${pkgs.kustomize}/bin/kustomize
    export KO_BIN=${my.ko}/bin/ko
  '';
}
