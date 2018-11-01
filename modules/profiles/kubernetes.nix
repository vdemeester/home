{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.containers.kubernetes;
in
{
  options = {
    profiles.containers.kubernetes = {
      enable = mkOption {
        default = true;
        description = "Enable kubernetes profile";
        type = types.bool;
      };
      minikube = {
        enable = mkOption {
          default = false;
          description = "Enable minikube";
          type = types.bool;
        };
        package = mkOption {
          default = pkgs.minikube;
          description = "Minikube package";
          type = types.package;
        };
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      profiles.containers.enable = true;
      home.packages = with pkgs; [
        cri-tools
        kail
        kubectl
        kustomize
        knctl
        kube-prompt
        kubectx
      ];
    }
    (mkIf cfg.minikube.enable {
      home.packages = with pkgs; [ cfg.minikube.package ];
    })
  ]);
}
