{ config, pkgs, ... }:

{
	services.xserver = {
			synaptics.enable = false;

			 config = ''
      			 Section "InputClass"
			         Identifier     "Enable libinput for TrackPoint"
				 MatchIsPointer "on"
				 Driver         "libinput"
				 Option         "ScrollMethod" "button"
				 Option         "ScrollButton" "8"
			 EndSection
    			 '';
			inputClassSections = [
					''
					Identifier "evdev touchpad off"
					MatchIsTouchpad "on"
					MatchDevicePath "/dev/input/event*"
					Driver "evdev"
					Option "Ignore" "true"
					''
			];
	};
}
