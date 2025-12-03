{ lib, ... }:
{
  imports = [
    ../../home/common/services/imapfilter.nix
  ];

  systemd.user.services.syncthing.Install.WantedBy = lib.mkForce [ "multi-user.target" ];
}
