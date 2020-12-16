{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.profiles.base;
in
{
  options = {
    config.profiles.base = {
      enable = mkEnableOption "base configuration";
    };
  };
  config = mkIf cfg.enable {
    # `nix-daemon` will hit the stack limit when using `nixFlakes`.
    systemd.services.nix-daemon.serviceConfig."LimitSTACK" = "infinity";
  };
}
