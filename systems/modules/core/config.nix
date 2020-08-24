{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles;
in
{
  options = {
    profiles.kubernetes = {
      enable = mkEnableOption "Enable Kubernetes profile";
    };
    profiles.openshift = {
      enable = mkEnableOption "Enable OpenShift profile";
      crc.enable = mkEnableOption "Enable CodeReady Containers";
    };
  };
}
