{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.cloud.google;
in
{
  options = {
    profiles.cloud.google = {
      enable = mkOption {
        default = false;
        description = "Enable google cloud profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [ google-cloud-sdk gcsfuse ]; #google-compute-engine
  };
}
