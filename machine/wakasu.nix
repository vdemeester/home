{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../profiles/laptop.nix
			../profiles/ssh.nix
			# ../profiles/virtualization.nix
			../profiles/dockerization.nix
			../profiles/dev.go.nix
			../profiles/dev.python.nix
			../profiles/mopidy.nix
			../location/docker.nix
			../location/home.nix
			../hardware/thinkpad-t460s.nix
		];

	security.pam.loginLimits = [
		{ domain = "@audio"; item = "memlock"; type = "-"; value = "unlimited"; }
		{ domain = "@audio"; item = "rtprio";  type = "-"; value = "99"; }
		{ domain = "@audio"; item = "nofile";  type = "-"; value = "99999"; }
	];

	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;

	boot.initrd.luks.devices = [
		{
			name = "root";
			device = "/dev/disk/by-uuid/e511e87f-a3b1-472a-bebb-c6cdd5154a16";
			preLVM = true;
			allowDiscards = true;
		}
	];

	services.redshift = {
		enable = true;
		brightness = { day = "1"; night = "0.9"; };
		latitude = "48.3";
		longitude = "7.5";
	};

	hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;
  /*
	hardware.bluetooth.extraConfig = "
[general]
Enable=Source,Sink,Media,Socket
";
  */
	hardware.trackpoint.enable = false;

	time.timeZone = "Europe/Paris";

	services.xserver.displayManager.slim.theme = pkgs.fetchurl {
		url = "https://github.com/vdemeester/slim-themes/raw/master/docker-key-theme-0.1.tar.xz";
		sha256 = "127893l1nzqya0g68k8841g5lm3hlnx7b3b3h06axvplc54a1jd8";
	};
}
