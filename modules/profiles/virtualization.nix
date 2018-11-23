{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.virtualization;
in
{
  options = {
    profiles.virtualization = {
      enable = mkOption {
        default = false;
        description = "Enable virtualization profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    virtualisation.libvirtd = {
      enable = true;
    };
    environment.systemPackages = with pkgs; [
      qemu
      vde2
    ];
  };
}
