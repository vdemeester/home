{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.buildkit;
in
{
  options = {
    profiles.buildkit = {
      enable = mkOption {
        default = false;
        description = "Enable buildkit profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    profiles.containerd.enable = true;
    environment.systemPackages = with pkgs; [
      buildkit
    ];
    virtualisation = {
      buildkitd= {
        enable = true;
        package = pkgs.buildkit;
        packages = [ pkgs.runc-edge pkgs.git ];
        extraOptions = "--oci-worker=false --containerd-worker=true";
      };
    };
  };
}
