# Virtualization configuration
{ config, pkgs, ... }:

{
	virtualisation = {
		virtualbox.host.enable = true;
	};
	networking.firewall.trustedInterfaces = [ "vboxnet0" ];
	environment = {
		systemPackages = with pkgs; [
			vagrant
		];
	};
}
