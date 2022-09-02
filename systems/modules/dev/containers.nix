{ config, lib, pkgs, ... }:

let
  cfg = config.modules.dev.containers;
  inherit (lib) mkEnableOption mkIf mkMerge mkOption types;
in
{
  options = {
    modules.dev.containers = {
      enable = mkEnableOption "Enable dev containers";
      docker = {
        enable = mkEnableOption "Enable docker containers";
        package = mkOption {
          default = pkgs.docker;
          description = "docker package to be used";
          type = types.package;
        };
        runcPackage = mkOption {
          default = pkgs.runc;
          description = "runc package to be used";
          type = types.package;
        };
      };
      podman = {
        enable = mkEnableOption "Enable podman containers";
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      virtualisation.containers.enable = true;
    }
    (mkIf cfg.docker.enable {
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
              enabled = true;
              platforms = [ "linux/amd64" "linux/arm64" ];
              namespace = "buildkit";
            };
            # FIXME: move to home
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
          package = cfg.docker.package;
          liveRestore = false;
          storageDriver = "overlay2";
          daemon.settings = {
            experimental = true;
            bip = "172.26.0.1/16";
            runtimes = {
              "docker-runc" = {
                path = "${cfg.docker.runcPackage}/bin/runc";
              };
            };
            default-runtime = "docker-runc";
            containerd = "/run/containerd/containerd.sock";
            features = { buildkit = true; };
            insecure-registries = [ "172.30.0.0/16" "192.168.12.0/16" "massimo.home:5000" "r.svc.home:5000" "r.svc.home" ];
            seccomp-profile = ./my-seccomp.json;
          };
        };
      };
      environment.systemPackages = with pkgs; [
        my.buildx
      ];
      networking.firewall.trustedInterfaces = [ "docker0" ];
    })
    (mkIf cfg.podman.enable {
      virtualisation.podman.enable = true;
    })
    (mkIf config.modules.profiles.work.redhat {
      # Red Hat specific setup for virtualisation (buildah, podman, skopeo)
      virtualisation = {
        containers = {
          registries = {
            search = [ "registry.fedoraproject.org" "registry.access.redhat.com" "registry.centos.org" "docker.io" "quay.io" ];
          };
          policy = {
            default = [{ type = "insecureAcceptAnything"; }];
            transports = {
              docker-daemon = {
                "" = [{ type = "insecureAcceptAnything"; }];
              };
            };
          };
        };
      };
    })
  ]);
}