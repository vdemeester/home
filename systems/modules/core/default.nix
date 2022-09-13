{ config, lib, pkgs, ... }:
{
  imports = [
    ./boot.nix
    ./config.nix
    ./nix.nix
    ./users.nix
    ./binfmt.nix
  ];

  boot = {
    cleanTmpDir = true;
  };
  # FIXME fix tmpOnTmpfs
  systemd.additionalUpstreamSystemUnits = [ "tmp.mount" ];

  security.sudo = {
    extraConfig = ''
      Defaults env_keep += SSH_AUTH_SOCK
    '';
  };
  systemd.services."status-email-root@" = {
    description = "status email for %i to vincent";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = ''
        ${pkgs.my.systemd-email}/bin/systemd-email vincent@demeester.fr %i
      '';
      User = "root";
      Environment = "PATH=/run/current-system/sw/bin";
    };
  };
}
