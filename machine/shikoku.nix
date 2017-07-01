{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/custom-packages.nix
			../configuration/common.nix
			../profiles/ssh.nix
			../profiles/laptop.nix
			../profiles/dev.nix
			../profiles/virtualization.nix
			../profiles/dockerization.nix
			../profiles/office.nix
			../profiles/gaming.nix
			../profiles/synergy-server.nix
			../location/home.nix
			../service/ssh-tunnel.nix
		];

	time.timeZone = "Europe/Paris";

	services = {
		xserver = {
			videoDrivers = [ "nvidia" ];
			displayManager = {
				sessionCommands = ''
xrandr --output HDMI-0 --off --output DP-4 --auto --dpi 96 &
'';
				slim.theme = pkgs.fetchurl {
					url = "https://github.com/vdemeester/slim-themes/raw/master/docker-paris-theme-0.1.tar.xz";
					sha256 = "1kp30qbxiwv0g4z6gsy2hjacpmm96lr2id1cdwizzf2lrash2hsi";
				};
			};
		};
	};

	hardware.bluetooth.enable = true;

	environment.etc."synergy-server.conf" = { text = ''
section: screens
	shikoku:
	honshu:
	wakasu:
end
section: links
	shikoku:
		left = wakasu
	honshu:
		right = wakasu
        wakasu:
		right = shikoku
		left = honshu
end
section: options
	keystroke(super+shift+left) = switchInDirection(left)
	keystroke(super+shift+right) = switchInDirection(right)
end
''; };
}
