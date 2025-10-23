_: {
  imports = [
    ../../home/common/dev/base.nix
    ../../home/common/dev/emacs.nix
    ../../home/common/dev/containers.nix
  ];
  systemd.user.services.syncthing.Install.WantedBy = [ "multi-user.target" ];
}
