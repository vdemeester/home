# Docker configuration
{ config, pkgs, ...}:

{
	imports = [
		../service/docker.nix
		# Remove once containerd module is upstream
		../service/containerd.nix
	];
	nixpkgs.config = {
		packageOverrides = self: with self; let
			fetchNixPkgs = { rev, sha256, owner, repo }:
				fetchFromGitHub {
					inherit sha256 rev owner repo;
				};
			dockerUnstablePkgs = import (fetchNixPkgs {
				owner = "NixOS";
				repo = "nixpkgs-channels";
				rev = "8ecadc12502d59fc8117ca0ed41ede010448fca4";
				sha256 = "102wvwixvnbkr547ay6psvv1x31001mb5y17ibkplyikb91xi2ak";
			}) {};
		in {
			inherit (dockerUnstablePkgs) docker docker-edge docker-proxy containerd runc tini docker-compose docker-machine;
		};
  };
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
