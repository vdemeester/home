{ config, pkgs, ... }:

{
	boot = {
		kernelParams = [
			# Kernel GPU Savings Options (NOTE i915 chipset only)
			"i915.enable_rc6=0" "i915.enable_fbc=1"
			"i915.lvds_use_ssc=0"
			"drm.debug=0" "drm.vblankoffdelay=1"
		];
		blacklistedKernelModules = [
			# Kernel GPU Savings Options (NOTE i915 chipset only)
			"sierra_net" "cdc_mbim" "cdc_ncm"
		];
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
	services.acpid.enable = true;
}
