{ lib, ... }:
{
  imports = [
    ../../home/common/desktop/passage.nix
    ../../home/common/services/imapfilter.nix
  ];

  systemd.user.services.syncthing.Install.WantedBy = lib.mkForce [ "multi-user.target" ];
}
