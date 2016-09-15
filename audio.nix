{ config, pkgs, ... }:

{
	hardware = {
		pulseaudio = {
			enable = true;
			support32Bit = true;
		};
	};

	environment.systemPackages = with pkgs; [
		pavucontrol
		pasystray
	];

	services.xserver.displayManager.sessionCommands = "${pkgs.pasystray}/bin/pasystray &";
}
