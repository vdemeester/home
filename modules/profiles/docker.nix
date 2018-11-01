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
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        docker
        docker-machine
        docker-machine-kvm
        docker-machine-kvm2
        python27Packages.docker_compose
      ];
    }
    (mkIf config.profiles.fish.enable {
      xdg.configFile."fish/conf.d/docker.fish".text = ''
        set -gx DOCKER_BUILDKIT 1
      '';
    })
  ]);
}
