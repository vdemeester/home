{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/common.nix
			../hardware/dell-latitude-e6540.nix
		];

	networking.hostName = "wakasu";
}
