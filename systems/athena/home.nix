{ lib, ... }:
{
  systemd.user.services.syncthing.Install.WantedBy = lib.mkForce [ "multi-user.target" ];
}
