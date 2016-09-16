{ config, pkgs, ... }:

{
	networking.networkmanager.enable = true;

	environment.systemPackages = with pkgs; [
		networkmanagerapplet
	];

	services.openssh.enable = true;
	services.openssh.startWhenNeeded = true;

	services.avahi.enable = true;
	services.avahi.nssmdns = true;
}
