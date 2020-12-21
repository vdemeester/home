{ config, lib, pkgs, ... }:

{
  home-manager.users.vincent = lib.mkIf config.profiles.laptop.enable {
    programs.autorandr.enable = true;
  };
}
