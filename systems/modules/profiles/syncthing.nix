{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.syncthing;
in
{
  options = {
    profiles.syncthing = {
      enable = mkEnableOption "Enable syncthing profile";
    };
  };
  config = mkIf cfg.enable {
    services.syncthing = {
      enable = true;
      user = "vincent";
      dataDir = "/home/vincent/.syncthing";
      configDir = "/home/vincent/.syncthing";
      openDefaultPorts = true;
    };
  };
}
