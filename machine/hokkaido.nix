{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/custom-packages.nix
			../configuration/common.nix
			../profiles/ssh.nix
			../profiles/laptop.nix
			../profiles/virtualization.nix
			../profiles/dockerization.nix
			../location/docker.nix
			../hardware/thinkpad-x220.nix
			../service/ssh-tunnel.nix
		];

	time.timeZone = "Europe/Paris";
	boot.loader.systemd-boot.enable = true;

	services.openssh.enable = true;
	services.openssh.forwardX11 = true;

	services.ssh-tunnel = {
		enable = true;
		localUser = "vincent";
		remoteHostname = "95.85.58.158";
		remotePort = 22;
		remoteUser = "vincent";
		bindPort = 2223;
	};
	services.xserver = {
		enable = true;
		videoDrivers = [ "intel" ];
		#xrandrHeads = [ "LVDS1" "DP1" ];
		monitorSection = ''
EndSection
Section "Monitor"
	Identifier "DP1"
	Option "PreferredMode" "2560x1080"
	Option "DPMS" "true"
	Option "RightOf" "LVDS1"
EndSection
Section "Monitor"
	Identifier "LVDS1"
	Option "Primary" "true"
	Option "DPMS" "true"
				'';
		deviceSection = ''
Option "Monitor-HDMI1" "HDMI1"
Option "Monitor-eDP1" "eDP1"
'';
		exportConfiguration = true;
		displayManager.slim.theme = pkgs.fetchurl {
			url = "https://github.com/vdemeester/slim-themes/raw/master/docker-nuage-theme-0.1.tar.xz";

			sha256 = "1ds7p3d8dn21bankgs68i53hqrj4d2abpk437h6dbjz36q1ys839";
		};
	};
}
