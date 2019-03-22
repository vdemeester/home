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
        default = true;
        description = "Enable podman tools";
        type = types.bool;
      };
      docker = mkOption {
        default = false;
        description = "Enable docker tools";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    profiles.docker.enable = cfg.docker;
    programs.podman.enable = cfg.podman;
    home.packages = with pkgs; [
      nur.repos.mic92.cntr
      nur.repos.vdemeester.ko
      nur.repos.vdemeester.go-containerregistry
      nur.repos.vdemeester.yak
      skopeo
    ];
    home.file."bin/kontain.me" = {
      text = ''
      #!${pkgs.stdenv.shell}
      command -v docker >/dev/null && {
        docker run -ti --rm kontain.me/ko/$@
      } || {
        podman run -ti --rm kontain.me/ko/$@
      }
      '';
      executable = true;
    };
  };
}
