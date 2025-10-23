{ pkgs, ... }:
{
  imports = [
    ../../home/common/dev/base.nix
    ../../home/common/dev/emacs.nix
    ../../home/common/dev/containers.nix
  ];
  services.ssh-agent.enable = true;
  systemd.user.services.syncthing.Install.WantedBy = [ "multi-user.target" ];

  home.packages = with pkgs; [
    gnumake
  ];
}
