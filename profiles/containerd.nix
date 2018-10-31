{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    cni
    cni-plugins
    containerd-edge
    runc-edge
    stellar
  ];
  virtualisation = {
    containerd = {
      enable = true;
      package = pkgs.containerd-edge;
      packages = [ pkgs.runc-edge ];
    };
  };
}
