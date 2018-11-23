{ pkgs, lib, ... }:

{
  imports = [
    ./profiles/fish.nix
    ./profiles/desktop.nix
    ./profiles/nix-config.nix
    ./profiles/nix-auto-update.nix
    ./profiles/laptop.nix
    ./programs/podman.nix
    ./services/syncthing.nix
    ./services/wireguard.client.nix
    ./virtualisation/buildkit.nix
    ./virtualisation/containerd.nix
    ./virtualisation/docker.nix
  ];
}
