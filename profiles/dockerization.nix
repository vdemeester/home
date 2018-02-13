# Docker configuration
{ config, pkgs, ...}:

{
	imports = [
		../service/docker.nix
		# Remove once containerd module is upstream
		../service/containerd.nix
	];
	environment.systemPackages = with pkgs; [
		python27Packages.docker_compose
		docker-machine
	];
	virtualisation = {
		containerd = {
			enable = true;
		};
		docker-edge = {
			enable = true;
			liveRestore = false;
			storageDriver = "overlay2";
			extraOptions = "--label=type=desktop --experimental --init --debug --add-runtime docker-runc=${pkgs.runc}/bin/runc --default-runtime=docker-runc --containerd=/run/containerd/containerd.sock";
		};
	};
	networking.firewall.trustedInterfaces = [ "docker0" ];
}
