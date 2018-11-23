{ pkgs, lib, ... }:

{
  imports = [
    ./profiles/buildkit.nix
    ./profiles/fish.nix
    ./profiles/containerd.nix
    ./profiles/desktop.nix
    ./profiles/docker.nix
    ./profiles/i18n.nix
    ./profiles/nix-config.nix
    ./profiles/nix-auto-update.nix
    ./profiles/laptop.nix
    ./profiles/users.nix
    ./profiles/virtualization.nix
    ./programs/podman.nix
    ./services/syncthing.nix
    ./services/wireguard.client.nix
    ./virtualisation/buildkit.nix
    ./virtualisation/containerd.nix
    ./virtualisation/docker.nix
  ];
}
