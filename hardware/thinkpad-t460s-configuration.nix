{ config, pkgs, ... }:

{
	imports =
		[
			./thinkpad.nix
		];
        boot = {
                kernelParams = [
      		        # Kernel GPU Savings Options (NOTE i915 chipset only)
			"i915.enable_rc6=1" "i915.enable_fbc=1"
			"i915.lvds_use_ssc=0"
      			"drm.debug=0" "drm.vblankoffdelay=1"
    		];
    	blacklistedKernelModules = [
      	        # Kernel GPU Savings Options (NOTE i915 chipset only)
      		"sierra_net" "cdc_mbim" "cdc_ncm"
    	];
};
	services = {
		acpid = {
			lidEventCommands = ''
if grep -q closed /proc/acpi/button/lid/LID/state; then
	date >> /tmp/i3lock.log
	DISPLAY=":0.0" XAUTHORITY=/home/fadenb/.Xauthority ${pkgs.i3lock}/bin/i3lock &>> /tmp/i3lock.log
fi
'';
		};
		tlp = {
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
# Make sure it uses the right hard drive
DISK_DEVICES="nvme0n1p3"
			'';
		};
	};
	services.xserver = {
		dpi = 128;
	};
}
