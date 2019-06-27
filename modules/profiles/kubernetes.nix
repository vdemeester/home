{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.containers.kubernetes;
in
{
  options = {
    profiles.containers.kubernetes = {
      enable = mkOption {
        default = false;
        description = "Enable kubernetes profile";
        type = types.bool;
      };
      containers= mkOption {
        default = true;
        description = "Enable containers profile alongside";
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
      profiles.containers.enable = cfg.containers;
      home.packages = with pkgs; [
        #cri-tools
        kail
        kustomize
        nur.repos.vdemeester.knctl
        kube-prompt
        kubectx
        nur.repos.vdemeester.tkn
      ];
    }
    (mkIf config.profiles.zsh.enable {
      programs.zsh.initExtra = ''
        source <(tkn completion zsh)
      '';
    })
    (mkIf cfg.minikube.enable {
      home.packages = with pkgs; [
        cfg.minikube.package
        docker-machine-kvm2
      ];
    })
    (mkIf (!config.profiles.containers.openshift.enable) {
      home.packages = with pkgs; [ kubectl ];
    })
  ]);
}
