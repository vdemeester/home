# Virtualization configuration
{ config, lib, pkgs, ... }:

{
	virtualisation = {
		virtualbox.host.enable = true;
	};
	boot.kernelModules = lib.mkBefore [ "kvm-intel" ];
	networking.firewall.trustedInterfaces = [ "vboxnet0" ];
	environment = {
		systemPackages = with pkgs; [
			vagrant
		];
	};
}
