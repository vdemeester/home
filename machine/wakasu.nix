{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/common.nix
			../hardware/thinkpad-t460s-configuration.nix
		];

	networking.hostName = "wakasu";

}
