{ config, lib, pkgs, ... }:

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
    home.packages = with pkgs; [ gnupg ];
    services = {
      gpg-agent = {
        enable = true;
        enableSshSupport = true;
        defaultCacheTtlSsh = 7200;
      };
    };
  };
}
