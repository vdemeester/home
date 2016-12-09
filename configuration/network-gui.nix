{ config, pkgs, ... }:

{
	networking.networkmanager.enable = true;

	environment.systemPackages = with pkgs; [
		networkmanagerapplet
	];

	services.avahi = {
		enable = true;
		nssmdns = true;
		publish.enable = true;
	};
}
