{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/custom-packages.nix
			../configuration/common.nix
			../hardware/thinkpad-t460s.nix
		];

	networking.hostName = "wakasu";

}
