# Common configuration for any laptops

{ configs, pkgs, ...}:

{
	imports = [
		./audio.nix
		./desktop.nix
	];

	environment.systemPackages = with pkgs; [
		acpi
		lm_sensors
		networkmanagerapplet
		powertop
	];

	services.xserver.displayManager.sessionCommands = ''
${pkgs.xss-lock}/bin/xss-lock i3lock -- -i $HOME/.background-image &
${pkgs.networkmanagerapplet}/bin/nm-applet &
	'';

	networking.networkmanager.enable = true;
}
