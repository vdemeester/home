{ config, pkgs, ... }:

{
  imports = [ ../service/containerd.nix ];
  environment.systemPackages = with pkgs; [
    cni
    cni-plugins
    containerd-edge
    runc-edge
  ];
  virtualisation = {
    containerd = {
      enable = true;
      package = pkgs.containerd-edge;
      packages = [ pkgs.runc-edge];
    };
  };
}
