{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.nix-auto-update;
in
{
  options = {
    profiles.nix-auto-update = {
      enable = mkOption {
        default = true;
        description = "Enable nix-auto-update profile";
        type = types.bool;
      };
      dates = mkOption {
        default = "weekly";
        description = "Specification (in the format described by systemd.time(7)) of the time at which the auto-update will run. ";
        type = types.str;
      };
      version = mkOption {
        default = "18.09";
        description = "System version (NixOS)";
        type = types.str;
      };
    };
  };
  config = mkIf cfg.enable {
    system = {
      stateVersion = cfg.version;
    };
    systemd.services.nixos-update = {
      description = "NixOS Upgrade";
      unitConfig.X-StopOnRemoval = false;
      serviceConfig.Type = "oneshot";
      environment = config.nix.envVars //
      { inherit (config.environment.sessionVariables) NIX_PATH;
        HOME = "/root";
      };
      path = [ pkgs.gnutar pkgs.xz pkgs.git config.nix.package.out ];
      script = ''
        cd /etc/nixos/
        git pull --autostash --rebase
        nix-channel --update
      '';
      startAt = cfg.dates;
    };
  };
}
