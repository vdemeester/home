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

	environment.etc."synergy-server.conf" = { text = ''
section: screens
	shikoku:
	wakasu:
end
section: links
	shikoku:
		left = wakasu
	wakasu:
		right = shikoku
end
section: options
	keystroke(super+shift+left) = switchInDirection(left)
	keystroke(super+shift+right) = switchInDirection(right)
end
''; };
}
