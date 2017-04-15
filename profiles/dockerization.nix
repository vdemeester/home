# Docker configuration
{ config, pkgs, ...}:

{
	environment.systemPackages = with pkgs; [
		python27Packages.docker_compose
		docker-machine
	];
	virtualisation = {
		docker = {
			enable = true;
			# experimental = true;
			liveRestore = false;
			storageDriver = "overlay2";
			extraOptions = "--label=type=desktop --experimental --init";
		};
	};
	networking.firewall.trustedInterfaces = [ "docker0" ];
}
