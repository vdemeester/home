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
      home.file.".local/share/applications/chos4.desktop".source = ../../assets/chos4.desktop;
      profiles.containers.enable = cfg.containers;
      home.packages = with pkgs; [
        #cri-tools
        kail
        kustomize
        kube-prompt
        kubectx
        nur.repos.vdemeester.ko
      ];
    }
    (mkIf config.profiles.zsh.enable {
      home.file."${config.programs.zsh.dotDir}/functions/_kubectl".source = ./assets/zsh/_kubectl;
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
