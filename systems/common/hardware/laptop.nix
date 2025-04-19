{ pkgs, ... }:
{
  # Some systctl options for all laptops
  boot.kernel.sysctl = {
    "kernel.nmi_watchdog" = 0;
    "vm.swappiness" = 10;
    "vm.dirty_ratio" = 25;
    "vm.dirty_background_ratio" = 10;
    "vm.dirty_writeback_centisecs" = 6000;
    "vm.dirty_expire_centisecs" = 6000;
  };

  environment.systemPackages = with pkgs; [
    acpi
    powertop
  ];

  # Run nix-gc only when on AC power
  systemd.services.nix-gc.unitConfig.ConditionACPower = true;

  services = {
    # When a laptop is docked or on external power, ignore the lid state (if the laptop is opened or closed)
    logind.extraConfig = ''
      HandleLidSwitchExternalPower=ignore
      HandleLidSwitchDocked=ignore
    '';
    power-profiles-daemon.enable = true;
  };

}
