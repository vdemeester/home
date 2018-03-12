# Common configuration for any laptops

{ configs, pkgs, ...}:

{
	imports = [
		./audio.nix
		./desktop.nix
	];

	environment.systemPackages = with pkgs; [
		acpi
		autorandr
		lm_sensors
		powertop
	];
}
