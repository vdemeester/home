# Docker configuration
{ config, pkgs, ...}:

{
	environment.systemPackages = with pkgs; [
		python27Packages.docker_compose
	];
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
