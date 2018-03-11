{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../profiles/ssh.nix
			../profiles/laptop.nix
			../profiles/dev.python.nix
			../profiles/dev.go.nix
			../profiles/virtualization.nix
			../profiles/dockerization.nix
			../profiles/office.nix
			../location/home.nix
			../hardware/dell-latitude-e6540.nix
			../service/ssh-tunnel.nix
		];

	time.timeZone = "Europe/Paris";
	boot.loader.systemd-boot.enable = true;

	security.pam.loginLimits = [
    		{ domain = "@audio"; item = "memlock"; type = "-"; value = "unlimited"; }
		{ domain = "@audio"; item = "rtprio";  type = "-"; value = "99"; }
		{ domain = "@audio"; item = "nofile";  type = "-"; value = "99999"; }
	];
	
	hardware.bluetooth.enable = true;
	services = {
		openssh = {
			enable = true;
			forwardX11 = true;
		};
		ssh-tunnel = {
			enable = true;
			localUser = "vincent";
			remoteHostname = "95.85.58.158";
			remotePort = 22;
			remoteUser = "vincent";
			bindPort = 2224;
		};
		printing = {
			enable = true;
			drivers = [ pkgs.gutenprint ];
		};
		xrdp.enable = true;
		xrdp.defaultWindowManager = "${pkgs.i3}/bin/i3";
		xserver = {
			enable = true;
			videoDrivers = [ "intel" ];
			exportConfiguration = true;
			displayManager.slim.theme = pkgs.fetchurl {
				url = "https://github.com/vdemeester/slim-themes/raw/master/docker-penguins-theme-0.1.tar.xz";
				sha256 = "1s0cfj1l9ay7y0ib68dnpdfkr1zwgr0b1s990ch786lxlajwwxpq";
			};
			monitorSection = ''
	Modeline "2560x1080_60.00"  230.00  2560 2720 2992 3424  1080 1083 1093 1120 -hsync +vsync
			'';
			deviceSection = ''
	Option "ModeValidation" "AllowNonEditModes"
			'';
			config = ''
	Section "Monitor"
		Identifier "eDP1"
		Option	   "ignore"	"true"
	EndSection
	Section "Monitor"
		Modeline "2560x1080_60.00"  230.00  2560 2720 2992 3424  1080 1083 1093 1120 -hsync +vsync
		Identifier "HDMI1"
	EndSection
			'';
		};
	};
	networking.firewall.allowedTCPPorts = [ 3389 2375 ];
}
