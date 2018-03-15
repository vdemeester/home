{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/custom-packages.nix
			../configuration/users.nix
			../profiles/server.nix
			../profiles/gitconfig.nix
			# ../profiles/dockerization.nix
			../profiles/avahi.nix
			../profiles/syncthing.nix
			../location/docker.nix
			../service/ssh-tunnel.nix
		];

	time.timeZone = "Europe/Paris";
	boot.loader.systemd-boot.enable = true;

	services.openssh.enable = true;

	networking.firewall.allowedTCPPorts = [ 80 443 ];

  services.virtualisation.docker = {
    enable = true;
    package = pkgs.docker-edge;
    storageDriver = "overlay2";
    extraOptions = "--experimental";
  };
	services.ssh-tunnel = {
		enable = true;
		localUser = "vincent";
		remoteHostname = "95.85.58.158";
		remotePort = 22;
		remoteUser = "vincent";
		bindPort = 2223;
	};
}
