{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.ssh;
in
{
  options = {
    profiles.ssh = {
      enable = mkEnableOption "Enable ssh profile";
      listenAddresses = mkOption {
        type = types.listOf types.str;
        default = [ ];
      };
      forwardX11 = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to allow X11 connections to be forwarded.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    services = {
      openssh = {
        enable = true;
        startWhenNeeded = false;
        forwardX11 = cfg.forwardX11;
        # listenAddresses = map
        # Move this for kerkouane only
        extraConfig = ''
          StreamLocalBindUnlink yes
          Match User nginx
            ChrootDirectory /var/www
            ForceCommand interfal-sftp
            AllowTcpForwarding no
            PermitTunnel no
            X11Forwarding no
        '';
      };
      sshguard.enable = true;
    };
    programs.mosh.enable = true;
  };
}
