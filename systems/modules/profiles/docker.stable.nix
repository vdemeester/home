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
          grpc = {
            # FIXME(vdemeester) move TCP behind an option (and not 0.0.0.0)
            address = [ "unix:///run/buildkit/buildkitd.sock" "tcp://0.0.0.0:1234" ];
          };
          worker.oci = {
            enabled = false;
          };
          worker.containerd = {
            enabled = true;
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
        extraOptions = "--experimental --add-runtime docker-runc=${cfg.runcPackage}/bin/runc --default-runtime=docker-runc --containerd=/run/containerd/containerd.sock";
      };
    };
    environment.etc."docker/daemon.json".text = ''
      {"features":{"buildkit": true}, "insecure-registries": ["172.30.0.0/16", "192.168.12.0/16", "massimo.home:5000", "r.svc.home:5000", "r.svc.home" ]}
    '';
    environment.systemPackages = with pkgs; [
      docker-buildx
    ];
    networking.firewall.trustedInterfaces = [ "docker0" ];
  };
}
