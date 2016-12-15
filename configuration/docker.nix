{ config, pkgs, ...}:

{
	virtualisation = {
		docker = {
			enable = true;
			# experimental = true;
			socketActivation = false;
			storageDriver = "overlay2";
			extraOptions = "--label=type=desktop";
		};
	};
	networking.firewall.trustedInterfaces = [ "docker0" ];

}
