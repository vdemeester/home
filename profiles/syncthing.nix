{ config, pkgs, ... }:

{
  services.syncthing = {
    enable = true;
    user = "vincent";
    dataDir = "/home/vincent/.syncthing";
    openDefaultPorts = true;
  };
}
