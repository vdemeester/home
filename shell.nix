let
  sources = import ./nix;
  pkgs = sources.nixpkgs { };
  nixos-unstable = sources.pkgs-unstable { };
  nixos = sources.pkgs { };
  sops-nix = sources.sops-nix;
in
pkgs.mkShell
{
  name = "nix-config";
  sopsPGPKeyDirs = [
    "./secrets/keys"
  ];
  nativeBuildInputs = [
    (pkgs.callPackage sops-nix { }).sops-import-keys-hook
  ];
  buildInputs = with pkgs; [
    cachix
    morph
    niv
    nixpkgs-fmt
    sops
    yq-go
  ];
  shellHook = ''
    export NIX_PATH="nixpkgs=${pkgs.path}:nixos=${nixos.path}:nixos-unstable=${nixos-unstable.path}"
    test -f .secrets && source .secrets || echo "no secrets"
    export QEMU_OPTS="-m 8096 -cpu host"
    export PATH="${builtins.toString ./.}/bin:$PATH"
    export REPO_ROOT="${builtins.toString ./.}"
  '';
}
