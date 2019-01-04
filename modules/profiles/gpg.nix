{ config, lib, ... }:

with lib;
let
  cfg = config.profiles.gpg;
in
{
  options = {
    profiles.gpg = {
      enable = mkOption {
        default = true;
        description = "Enable gpg profile and configuration";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    services = {
      gpg-agent = {
        enable = true;
        enableSshSupport = true;
        defaultCacheTtlSsh = 7200;
      };
    };
  };
}
