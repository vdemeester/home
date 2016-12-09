{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/users.nix
			../configuration/system.nix
			../configuration/network.nix
			# ../configuration/docker.nix
			../configuration/custom-packages.nix
		];

	networking.hostName = "kobe";

}
