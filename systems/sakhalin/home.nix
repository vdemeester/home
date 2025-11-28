{ pkgs, ... }:
{
  systemd.user.services.syncthing.Install.WantedBy = [ "multi-user.target" ];
  home.packages = with pkgs; [
    download-kiwix-zim
  ];
}
