{ pkgs, lib, ... }:

{
  imports = [
    ./programs/podman.nix
    ./services/wireguard.client.nix
    ./virtualisation/buildkit.nix
    ./virtualisation/containerd.nix
    ./virtualisation/docker.nix
  ];
}
