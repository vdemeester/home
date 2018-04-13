# Virtualization configuration
{ config, pkgs, ... }:

{
	virtualisation = {
		libvirtd = {
			enable = true;
			qemuVerbatimConfig = ''
namespaces = []
dynamic_ownership = 0
'';
			extraConfig = ''
dynamic_ownership = 0
'';
		};
	};
}
