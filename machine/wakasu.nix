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
			# ../profiles/dockerization.nix
			../service/docker.nix
			../profiles/dev.nix
			../profiles/dev.python.nix
			../location/docker.nix
			../location/home.nix
			../service/containerd.nix
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

	# FIXME(vdemeester) remove when containerd module is merged upstream
	virtualisation = {
		containerd = {
			enable = true;
		};
		docker-edge = {
			enable = true;
			liveRestore = false;
			storageDriver = "overlay2";
			extraOptions = "--label=type=desktop --experimental --init --debug --add-runtime docker-runc=${pkgs.runc}/bin/runc --default-runtime=docker-runc --containerd=/run/containerd/containerd.sock";
		};
	};
	networking.firewall.trustedInterfaces = [ "docker0" ];

	hardware.bluetooth.enable = true;
	hardware.trackpoint.enable = false;

	environment.systemPackages = with pkgs; [
		autorandr
		zoom-us
		# FIXME(vdemeester) remove when containerd module is merged upstream
		python27Packages.docker_compose
		docker-machine
	];

	time.timeZone = "Europe/Paris";

	services.xserver.displayManager.slim.theme = pkgs.fetchurl {
		url = "https://github.com/vdemeester/slim-themes/raw/master/docker-key-theme-0.1.tar.xz";
		sha256 = "127893l1nzqya0g68k8841g5lm3hlnx7b3b3h06axvplc54a1jd8";
	};
	networking.firewall.allowedTCPPorts = [ 8080 8000 ];
}
