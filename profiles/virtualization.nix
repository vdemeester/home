# Virtualization configuration
{ config, pkgs, ... }:

{
	virtualisation = {
		virtualbox.host.enable = true;
		libvirtd = {
			enable = true;
			enableKVM = true;
		};
	};
	networking.firewall.trustedInterfaces = [ "vboxnet0" ];
	environment = {
		systemPackages = with pkgs; [
			vagrant
		];
	};
}
