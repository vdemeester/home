{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.ssh;
in
{
  options = {
    profiles.ssh = {
      enable = mkOption {
        default = false;
        description = "Enable ssh profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    services = {
      openssh = {
        enable = true;
        startWhenNeeded = false;
      };
    };
    programs.mosh.enable = true;
  };
}
