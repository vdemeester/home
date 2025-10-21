{ pkgs, ... }:
{
  imports = [
    ../../home/common/dev/base.nix
  ];
  services.ssh-agent.enable = true;
  systemd.user.services.syncthing.Install.WantedBy = [ "multi-user.target" ];

  home.packages = with pkgs; [
    gnumake
  ];
}
