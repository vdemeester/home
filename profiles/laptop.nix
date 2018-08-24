{ configs, pkgs, ...}:

{
  imports = [
    ./audio.nix
    ./desktop.nix
  ];

  environment.systemPackages = with pkgs; [
    lm_sensors
    powertop
    acpi
  ];
}
