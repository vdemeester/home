{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf mkDefault mkOverride;
  cfg = config.profiles.base;
in
{
  options = {
    profiles.base = {
      enable = mkEnableOption "base configuration";
    };
  };
  config = mkIf cfg.enable {
    # Use systemd-boot by default, can be overridden by configurations
    boot.loader.systemd-boot.enable = mkDefault true;
    # `nix-daemon` will hit the stack limit when using `nixFlakes`.
    systemd.services.nix-daemon.serviceConfig."LimitSTACK" = "infinity";
    environment = {
      variables = {
        EDITOR = mkOverride 0 "vim";
      };
    };
    # Make sure we never remove SSH_AUTH_SOCK when reseting env through sudo
    security.sudo.extraConfig = ''
      Defaults env_keep += SSH_AUTH_SOCK
    '';
    # Setup a *mailer* in case of failure in systemd
    systemd.services."status-email-root@" = {
      description = "status email for %i to vincent";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = ''
          ${pkgs.systemd-email}/bin/systemd-email vincent@demeester.fr %i
        '';
        User = "root";
        Environment = "PATH=/run/current-system/sw/bin";
      };
    };
  };
}
