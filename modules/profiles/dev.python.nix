{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.dev.python;
in
{
  options = {
    profiles.dev.python = {
      enable = mkOption {
        default = false;
        description = "Enable python development profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    profiles.dev.enable = true;
    home.packages = with pkgs; [
      python3
      python36Packages.virtualenv
      python36Packages.pip-tools
      python36Packages.tox
      pipenv
    ];
  };
}
