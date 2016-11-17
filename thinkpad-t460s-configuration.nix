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
	
	boot.extraModprobeConfig = ''
	options snd_hda_intel power_save=1
	# options i915 enable_rc6=0
	'';
}
