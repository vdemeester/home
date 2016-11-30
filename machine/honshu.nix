{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/custom-packages.nix
			../configuration/common.nix
			../hardware/dell-latitude-e6540.nix
		];

	networking.hostName = "honshu";
	
	services.xserver.displayManager.slim.theme = pkgs.fetchurl {
						url = "https://github.com/vdemeester/slim-themes/raw/master/docker-penguins-theme-0.1.tar.xz";
						sha256 = "1s0cfj1l9ay7y0ib68dnpdfkr1zwgr0b1s990ch786lxlajwwxpq";
						};
}
