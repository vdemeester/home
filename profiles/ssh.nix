{ config, pkgs, ... }:

{
  services = {
    openssh = {
      enable = true;
      startWhenNeeded = false;
    };
  };
}
