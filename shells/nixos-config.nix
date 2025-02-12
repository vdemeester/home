{ pkgs, inputs }:

pkgs.mkShell
{
  name = "NixOS config";
  buildInputs = with pkgs; [
    cachix
    git
    nixpkgs-fmt
    sops
    yq-go
  ];
  shellHook = ''
    test -f .secrets && source .secrets || echo "no secrets"
    export QEMU_OPTS="-m 8096 -cpu host"
    export PATH="${builtins.toString ./.}/bin:$PATH"
    export REPO_ROOT="${builtins.toString ./.}"
  '';
}
