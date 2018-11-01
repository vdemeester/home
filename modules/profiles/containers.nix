{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.containers;
in
{
  options = {
    profiles.containers = {
      enable = mkOption {
        default = false;
        description = "Enable containers profile";
        type = types.bool;
      };
      podman = mkOption {
        default = podman;
        description = "Enable podman tools";
        type = types.bool;
      };
      docker = mkOption {
        default = false;
        description = "Enable docker tools";
        type = types.bool;
      };
    };
    config = mkIf cfg.enable {
      profiles.docker.enable = cfg.docker;
      profiles.podman.enalbe = cfg.podman || !cfg.docker;
      home.packages = with pkgs; [
        go-containerregistry
        skopeo
      ];
    };
  };
}
