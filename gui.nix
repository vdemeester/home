{ config, pkgs, ... }:

{
	services = {
		xserver = {
			enable = true;
			synaptics = {
				enable = true;
				twoFingerScroll = true;
				tapButtons = true;
			};
			windowManager = {
				i3 = {
					enable = true;
				};
			};
			displayManager = {
				sessionCommands = "${pkgs.networkmanagerapplet}/bin/nm-applet &";
			};
		};
	};
}
