{ config, pkgs, ... }:

{
  imports = [ ../service/containerd.nix ];
  environment.systemPackages = with pkgs; [ runc ];
  virtualisation = {
    containerd = {
      enable = true;
      packages = [ pkgs.runc ];
    };
  };
}
