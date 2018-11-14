{ config, pkgs, ... }:

{
  services.syncthing-edge = {
    enable = true;
    user = "vincent";
    dataDir = "/home/vincent/.syncthing";
    openDefaultPorts = true;
  };
}
