{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/custom-packages.nix
			../configuration/common.nix
			../profiles/audio.nix
			../hardware/thinkpad-x220.nix
			../service/ssh-tunnel.nix
		];

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
	
	services.xserver.displayManager.slim.theme = pkgs.fetchurl {
						url = "https://github.com/vdemeester/slim-themes/raw/master/docker-nuage-theme-0.1.tar.xz";
						sha256 = "1ds7p3d8dn21bankgs68i53hqrj4d2abpk437h6dbjz36q1ys839";
						};
}
