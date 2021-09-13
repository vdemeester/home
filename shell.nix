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
    nixos-generators
    nixpkgs-fmt
    sops
  ];
  shellHook = ''
    export NIX_PATH="nixpkgs=${pkgs.path}:nixos=${nixos.path}:nixos-unstable=${nixos-unstable.path}"
  '';
}
