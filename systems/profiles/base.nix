{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption;
  cfg = config.profiles.base;
in
{
  options = {
    enable = mkEnableOption "base configuration";
  };
  config = {

    # `nix-daemon` will hit the stack limit when using `nixFlakes`.
    systemd.services.nix-daemon.serviceConfig."LimitSTACK" = "infinity";
  };
}
