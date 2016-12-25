{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/custom-packages.nix
			../configuration/common.nix
			../profiles/audio.nix
			../hardware/dell-latitude-e6540.nix
		];

	services = {
		xserver = {
			enable = true;
			videoDrivers = [ "intel" ];
			monitorSection = ''
EndSection
Section "Monitor"
	Identifier "HDMI1"
	Modeline "2560x1080_60.00"  230.00  2560 2720 2992 3424  1080 1083 1093 1120 -hsync +vsync
	Option "PreferredMode" "2560x1080_60.00"
	Option  "Primary" "true"
	Option "DPMS" "true"
EndSection
Section "Monitor"
	Identifier "eDP1"
	Option "RightOf" "HDMI1"
	Option "DPMS" "true"
				'';
			deviceSection = ''
				Option "Monitor-HDMI1" "HDMI1"
				Option "Monitor-eDP1" "eDP1"
'';
			exportConfiguration = true;
			displayManager.slim.theme = pkgs.fetchurl {
						url = "https://github.com/vdemeester/slim-themes/raw/master/docker-penguins-theme-0.1.tar.xz";
						sha256 = "1s0cfj1l9ay7y0ib68dnpdfkr1zwgr0b1s990ch786lxlajwwxpq";
						};

		};
};
}
