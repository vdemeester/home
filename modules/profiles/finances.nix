{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.finances;
in
{
  options = {
    profiles.finances = {
      enable = mkOption {
        default = false;
        description = "Enable fincances profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [ ledger ];
  };
}
