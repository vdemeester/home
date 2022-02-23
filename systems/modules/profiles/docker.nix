{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.docker;
in
{
  options = {
    profiles.docker = {
      enable = mkEnableOption "Enable docker profile";
      package = mkOption {
        default = pkgs.docker-edge;
        description = "docker package to be used";
        type = types.package;
      };
      runcPackage = mkOption {
        default = pkgs.runc;
        description = "runc package to be used";
        type = types.package;
      };
    };
  };
  config = mkIf cfg.enable {
    virtualisation = {
      containerd = {
        enable = true;
      };
      buildkitd = {
        enable = true;
        settings = {
          worker.oci = {
            enabled = false;
          };
          worker.containerd = {
            enable = true;
            platforms = [ "linux/amd64" "linux/arm64" ];
            namespace = "buildkit";
          };
          registry = {
            "r.svc.home:5000" = {
              http = true;
              insecure = true;
            };
            "r.svc.home" = {
              http = true;
              insecure = true;
            };
          };
        };
      };
      docker = {
        enable = true;
        package = cfg.package;
        liveRestore = false;
        storageDriver = "overlay2";
        daemon.settings = {
          experimental = true;
          runtimes = {
            "docker-runc" = {
              path = "${cfg.runcPackage}/bin/runc";
            };
          };
          default-runtime = "docker-runc";
          containerd = "/run/containerd/containerd.sock";
          features = { buildkit = true; };
          insecure-registries = [ "172.30.0.0/16" "192.168.12.0/16" "massimo.home:5000" "r.svc.home:5000" "r.svc.home" ];
          seccomp-profile = ./docker/my-seccomp.json;
        };
      };
    };
    environment.systemPackages = with pkgs; [
      my.buildx
    ];
    networking.firewall.trustedInterfaces = [ "docker0" ];
  };
}
