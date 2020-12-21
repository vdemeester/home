{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkEnableOption mkDefault;
  cfg = config.profiles.desktop;
in
{
  options = {
    profiles.desktop = {
      enable = mkEnableOption "desktop configuration";
    };
  };
  config = mkIf cfg.enable {
    boot = {
      # /tmp to be tmpfs
      tmpOnTmpfs = true;
      # Enable Plymouth on desktops
      plymouth.enable = true;
    };
    nix = {
      # Enable SSH-serving nix packages
      sshServe.enable = mkDefault true;
    };

    # Make `/run/user/X` larger.
    services.logind.extraConfig = ''
      RuntimeDirectorySize=20%
    '';
  };
}
