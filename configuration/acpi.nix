{ config, pkgs, ... }:

{
	environment.systemPackages = with pkgs; [
		acpi
		lm_sensors
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
DISK_DEVICES="nvme0n1p3"
			'';
		};
	};
}
