{ config, pkgs, ... }:

{
	hardware = {
		pulseaudio = {
			enable = true;
			support32Bit = true;
		};
	};
	sound.enableMediaKeys = true;

	environment.systemPackages = with pkgs; [
		apulse       # allow alsa application to use pulse
		pavucontrol  # pulseaudio volume control
		pasystray    # systray application
	];

	# We assume xserver runs when pulseaudio does
	services.xserver.displayManager.sessionCommands = "${pkgs.pasystray}/bin/pasystray &";
}
