{ config, pkgs, ... }:

{
  imports = [ ../service/containerd.nix ];
  environment.systemPackages = with pkgs; [
    cni
    # cni-plugins
    containerd
    runc
  ];
  virtualisation = {
    containerd = {
      enable = true;
      packages = [ pkgs.runc ];
    };
  };
}
