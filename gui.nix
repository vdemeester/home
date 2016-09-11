{ config, pkgs, ... }:

{
        environment = {
                    systemPackages = with pkgs; [
                                   firefox
                                   i3status
                                   dmenu
                                   emacs
                                   xdg-user-dirs
                    ];
        };

	services = {
		xserver = {
			enable = true;
			vaapiDrivers = [ pkgs.vaapiIntel ];
			xrandrHeads = [ "HDMI1" "VGA2" "eDP1" ];
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
