{ config, pkgs, ... }:

{
	environment.systemPackages = with pkgs; [
		acpi
		lm_sensors
	];

	services = {
		acpid = {
			enable = true;
		};
	};
}
