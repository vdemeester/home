{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.laptop;
in
{
  options = {
    profiles.laptop = {
      enable = mkEnableOption "Enable laptop profile";
    };
  };
  config = mkIf cfg.enable {
    warnings = [ "The option 'profiles.laptop' is deprecated, use 'modules.hardware.laptop' instead" ];
    # Use modules.hardware.enable instead
    modules.hardware.laptop.enable = true;
  };
}
