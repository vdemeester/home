{ config, pkgs, ... }:

{
	hardware = {
		pulseaudio = {
			enable = true;
			support32Bit = true;
			zeroconf = {
				discovery.enable = true;
				publish.enable = true;
			};
			tcp.enable = true;
			package = pkgs.pulseaudioFull;
		};
	};
	sound.mediaKeys.enable = true;

	# spotify
	networking.firewall.allowedTCPPorts = [ 57621 57622 ];
	networking.firewall.allowedUDPPorts = [ 57621 57622 ];
	
	environment.systemPackages = with pkgs; [
		apulse       # allow alsa application to use pulse
		pavucontrol  # pulseaudio volume control
		pasystray    # systray application
		playerctl
	];

	# We assume xserver runs when pulseaudio does
	services.xserver.displayManager.sessionCommands = "${pkgs.pasystray}/bin/pasystray &";
}
