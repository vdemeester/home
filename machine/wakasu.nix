{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/custom-packages.nix
			../configuration/common.nix
			../profiles/laptop.nix
			../profiles/ssh.nix
			../profiles/virtualization.nix
			../profiles/dockerization.nix
			../profiles/dev.nix
			../location/docker.nix
			../location/home.nix
			../hardware/thinkpad-t460s.nix
		];

	#services.synergy.client = {
	#	enable = true;
	#	autoStart = false;
	#	screenName = "wakasu";
	#	serverAddress = "hokkaido.local";
	#};

	security.pam.loginLimits = [
		{ domain = "@audio"; item = "memlock"; type = "-"; value = "unlimited"; }
		{ domain = "@audio"; item = "rtprio";  type = "-"; value = "99"; }
		{ domain = "@audio"; item = "nofile";  type = "-"; value = "99999"; }
	];
	hardware.bluetooth.enable = true;
	
	time.timeZone = "Europe/Paris";

	services.xserver.displayManager.slim.theme = pkgs.fetchurl {
						url = "https://github.com/vdemeester/slim-themes/raw/master/docker-key-theme-0.1.tar.xz";
						sha256 = "127893l1nzqya0g68k8841g5lm3hlnx7b3b3h06axvplc54a1jd8";
						};
        networking.firewall.allowedTCPPorts = [ 8080 8000 ];
}
