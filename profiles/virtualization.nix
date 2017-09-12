# Virtualization configuration
{ config, pkgs, ... }:

{
	virtualisation = {
		virtualbox.host.enable = true;
		libvirtd = {
			enable = true;
			enableKVM = true;
			qemuVerbatimConfig = ''
namespaces = []
dynamic_ownership = 0
'';
			extraConfig = ''
dynamic_ownership = 0
'';
		};
	};
	networking.firewall.trustedInterfaces = [ "vboxnet0" ];
	environment = {
		systemPackages = with pkgs; [
			vagrant
			runc
			containerd
		];
	};
}
