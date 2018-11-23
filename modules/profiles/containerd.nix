{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.containerd;
in
{
  options = {
    profiles.containerd = {
      enable = mkOption {
        default = false;
        description = "Enable containerd profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
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
  };
}
