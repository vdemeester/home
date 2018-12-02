{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.docker;
in
{
  options = {
    profiles.docker = {
      enable = mkOption {
        default = false;
        description = "Enable docker profile";
        type = types.bool;
      };
      package = mkOption {
        default = pkgs.docker-edge;
        description = "docker package to be used";
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
    virtualisation = {
      docker = {
        enable = true;
        package = cfg.package;
        liveRestore = false;
        storageDriver = "overlay2";
        extraOptions = "--label=type=desktop --experimental --init --debug --add-runtime docker-runc=${cfg.runcPackage}/bin/runc --default-runtime=docker-runc --containerd=/run/containerd/containerd.sock --insecure-registry 172.30.0.0/16";
      };
    };
    networking.firewall.trustedInterfaces = [ "docker0" ];
  };
}
