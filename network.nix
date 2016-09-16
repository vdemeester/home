{ config, pkgs, ... }:

{
	networking.networkmanager.enable = true;

	environment.systemPackages = with pkgs; [
		networkmanagerapplet
	];

	services.openssh.enable = true;

	services.avahi.enable = true;
	service.avahi.nssmdns = true;
}
