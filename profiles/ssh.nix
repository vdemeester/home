{ config, pkgs, ... }:

{
  services = {
    openssh = {
      enable = true;
      startWhenNeeded = false;
    };
  };
  programs.mosh.enable = true;
}
