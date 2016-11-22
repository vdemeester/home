{ config, pkgs, ... }:

{
        boot = {
                kernelParams = [
      		        # Kernel GPU Savings Options (NOTE i915 chipset only)
			#"i915.enable_rc6=1" "i915.enable_fbc=1"
			#"i915.lvds_use_ssc=0"
      			"drm.debug=0" "drm.vblankoffdelay=1"
    		];
    	blacklistedKernelModules = [
      	        # Kernel GPU Savings Options (NOTE i915 chipset only)
      		"sierra_net" "cdc_mbim" "cdc_ncm"
    	];
};
        environment.systemPackages = with pkgs; [
	        linuxPackages_4_8.tp_smapi
	];
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
# CPU optimizations
CPU_SCALING_GOVERNOR_ON_AC=performance
CPU_SCALING_GOVERNOR_ON_BAT=powersave
CPU_MIN_PERF_ON_AC=0
CPU_MAX_PERF_ON_AC=100
CPU_MIN_PERF_ON_BAT=0
CPU_MAX_PERF_ON_BAT=50
# DEVICES (wifi, ..)
DEVICES_TO_DISABLE_ON_STARTUP="bluetooth"
DEVICES_TO_ENABLE_ON_AC="bluetooth wifi wwan"
DEVICES_TO_DISABLE_ON_BAT="bluetooth"
# Network management
DEVICES_TO_DISABLE_ON_LAN_CONNECT=""
DEVICES_TO_DISABLE_ON_WIFI_CONNECT=""
DEVICES_TO_DISABLE_ON_WWAN_CONNECT=""
DEVICES_TO_ENABLE_ON_LAN_DISCONNECT=""
DEVICES_TO_ENABLE_ON_WIFI_DISCONNECT=""
DEVICES_TO_ENABLE_ON_WWAN_DISCONNECT="" 
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
	'';
	systemd.services.tune-powermanagement = {
    	        description = "Tune Powermanagement";
    		serviceConfig.Type = "oneshot";
    		serviceConfig.RemainAfterExit = true;
    		wantedBy = [ "multi-user.target" ];
    		unitConfig.RequiresMountsFor = "/sys";
    		script = ''
      		echo '1500' > '/proc/sys/vm/dirty_writeback_centisecs'
    		'';
	};
}
