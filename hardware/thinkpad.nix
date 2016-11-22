{ config, pkgs, ... }:

{
	environment.systemPackages = with pkgs; [
		linuxPackages_4_8.tp_smapi
	];
	services = {
		acpid = {
			enable = true;
		};
		tlp = {
			enable = true;
		};
	};
	services.xserver = {
		synaptics.enable = false;
		config =
		''
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
