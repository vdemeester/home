{ config, pkgs, ... }:

{

	services = {
		acpid = {
			enable = true;
			lidEventCommands = ''
if grep -q closed /proc/acpi/button/lid/LID/state; then
	date >> /tmp/i3lock.log
	DISPLAY=":0.0" XAUTHORITY=/home/fadenb/.Xauthority ${pkgs.i3lock}/bin/i3lock &>> /tmp/i3lock.log
fi
'';
		};
		tlp = {
			enable = true;
			extraConfig = ''
DISK_DEVICES="nvme0n1p3"
			'';
		};
	};
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
