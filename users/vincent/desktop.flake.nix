{ config, lib, pkgs, ... }:

{
  home-manager.users.vincent = lib.mkIf config.profiles.desktop.enable (import ./desktop);
}
