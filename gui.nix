{ config, pkgs, ... }:

{
	services = {
		xserver = {
			enable = true;
			vaapiDrivers = [ pkgs.vaapiIntel ];
			synaptics = {
				enable = true;
				twoFingerScroll = true;
				tapButtons = true;
			};
			windowManager = {
				i3 = {
					enable = true;
				};
				default = "i3";
			};
			displayManager = {
				sessionCommands = "${pkgs.networkmanagerapplet}/bin/nm-applet &";
			};
		};
	};

	fonts = {
	      enableFontDir = true;
	      enableGhostscriptFonts = true;
	      fonts = with pkgs; [
	      	    corefonts
		    inconsolata
		    dejavu_fonts
		    ubuntu_font_family
		    unifont
		    google-fonts
		    symbola
		    fira
		    fira-code
		    fira-mono
		    font-droid
	      ];
	};
}
