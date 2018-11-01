{ pkgs, lib, ... }:

{
  imports = [
    ./profiles/fish.nix
    #./profiles/desktop.nix
    #./profiles/laptop.nix
    ./programs/podman.nix
    ./services/wireguard.client.nix
    ./virtualisation/buildkit.nix
    ./virtualisation/containerd.nix
    ./virtualisation/docker.nix
  ];
}
