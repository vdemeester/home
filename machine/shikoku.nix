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
			../profiles/office.nix
			../profiles/gaming.nix
			../profiles/synergy-server.nix
			../location/home.nix
			../service/ssh-tunnel.nix
		];

	services = {
		xserver = {
			videoDrivers = [ "nvidia" ];
		};
	};

	hardware.bluetooth.enable = true;

	environment.etc."synergy-server.conf" = { text = ''
section: screens
	shikoku:
	hokkaido:
end
section: links
	shikoku:
		left = hokkaido
	hokkaido:
		right = shikoku
end
section: options
	keystroke(super+shift+left) = switchInDirection(left)
	keystroke(super+shift+right) = switchInDirection(right)
end
''; };

	services.xserver.displayManager.slim.theme = pkgs.fetchurl {
						url = "https://github.com/vdemeester/slim-themes/raw/master/docker-paris-theme-0.1.tar.xz";
						sha256 = "1kp30qbxiwv0g4z6gsy2hjacpmm96lr2id1cdwizzf2lrash2hsi";
						};
}
