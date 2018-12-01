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
        default = pkgs.nur.repos.vdemeester.containerd;
        description = "containerd package to be used";
        type = types.package;
      };
      runcPackage = mkOption {
        default = pkgs.nur.repos.vdemeester.runc;
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
