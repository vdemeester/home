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
      package = mkOption {
      default = pkgs.containerd-edge;
        description = "containerd package to be used";
        type = types.package;
      };
      runcPackage = mkOption {
        default = pkgs.runc-edge;
        description = "runc package to be used";
        type = types.package;
      };
    };
  };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      cni
      cni-plugins
      cfg.package
      cfg.runcPackage
      stellar
    ];
    virtualisation = {
      containerd = {
        enable = true;
        package = cfg.package;
        packages = [ cfg.runcPackage ];
      };
    };
  };
}
