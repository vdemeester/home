{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf mkOption types;
  cfg = config.modules.hardware.laptop;
in
{
  options = {
    modules.hardware.laptop = {
      enable = mkEnableOption "Enable laptop profile";
    };
  };
  config = mkIf cfg.enable {
    # Some systctl options for all laptops
    boot.kernel.sysctl = {
      "vm.swappiness" = 10;
      "vm.dirty_ratio" = 25;
      "vm.dirty_background_ratio" = 10;
      "vm.dirty_writeback_centisecs" = 5000;
      "vm.dirty_expire_centisecs" = 5000;
    };
    # Packages that are always useful for laptops
    environment.systemPackages = with pkgs; [
      lm_sensors
      powertop
      acpi
    ];
    # Run nix-gc only when on AC power
    systemd.services.nix-gc.unitConfig.ConditionACPower = true;
    # When a laptop is docked, ignor the lid state (if the laptop is opened or closed)
    services = {
      logind.extraConfig = ''
        HandleLidSwitchDocked=ignore
      '';
    };
  };
}
