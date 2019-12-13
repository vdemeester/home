{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.containers.kubernetes;
in
{
  options = {
    profiles.containers.kubernetes = {
      enable = mkEnableOption "Enable kubernetes profile";
      containers= mkOption {
        default = true;
        description = "Enable containers profile alongside";
        type = types.bool;
      };
      krew = {
        enable = mkEnableOption "Enable krew";
      };
      kind = {
        enable = mkEnableOption "Enable kind";
      };
      minikube = {
        enable = mkEnableOption "Enable minikube";
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
    (mkIf cfg.krew.enable {
      home.packages = with pkgs; [ nur.repos.vdemeester.krew ];
    })
    (mkIf config.profiles.zsh.enable {
      home.file."${config.programs.zsh.dotDir}/functions/_kubectl".source = ./assets/zsh/_kubectl;
    })
    (mkIf cfg.minikube.enable {
      home.packages = with pkgs; [
        cfg.minikube.package
        docker-machine-kvm2
      ];
    })
    (mkIf cfg.kind.enable {
      home.packages = with pkgs; [
        kind
      ];
    })
    (mkIf (!config.profiles.containers.openshift.enable) {
      home.packages = with pkgs; [ kubectl ];
    })
  ]);
}
