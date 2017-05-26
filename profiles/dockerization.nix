# Docker configuration
{ config, pkgs, ...}:

{
	imports = [
		../service/docker.nix
	];
	environment.systemPackages = with pkgs; [
		python27Packages.docker_compose
		docker-machine
	];
	virtualisation = {
		docker-edge = {
			enable = true;
			# experimental = true;
			liveRestore = false;
			storageDriver = "overlay2";
			extraOptions = "--label=type=desktop --experimental --init --debug";
		};
	};
	networking.firewall.trustedInterfaces = [ "docker0" ];
}
