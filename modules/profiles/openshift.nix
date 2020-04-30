{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.containers.openshift;
in
{
  options = {
    profiles.containers.openshift = {
      enable = mkEnableOption "Enable openshift profile";
      package = mkOption {
        default = pkgs.openshift;
        description = "Openshift package";
        type = types.package;
      };
      minishift = {
        enable = mkEnableOption "Enable minishift";
        package = mkOption {
          default = pkgs.minishift;
          description = "Minishift package";
          type = types.package;
        };
      };
    };
  };
  config = mkIf cfg.enable (
    mkMerge [
      {
        profiles.containers.kubernetes.enable = true;
        home.packages = with pkgs; [
          nur.repos.vdemeester.s2i
          cfg.package
        ];
      }
      (
        mkIf cfg.minishift.enable {
          home.packages = with pkgs; [
            cfg.minishift.package
            docker-machine-kvm
            docker-machine-kvm2
          ];
        }
      )
    ]
  );
}
