{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/users.nix
			../configuration/system.nix
			../configuration/network.nix
			# ../configuration/docker.nix
			../configuration/custom-packages.nix
			../service/ssh-tunnel.nix
		];

	# Enable the OpenSSH daemon.
	services.openssh.enable = true;
	users.users.root.openssh.authorizedKeys.keys =
		with import ../ssh-keys.nix; [ wakasu hokkaido ];

	services.ssh-tunnel = {
		enable = true;
		localUser = "vincent";
		remoteHostname = "95.85.58.158";
		remotePort = 22;
		remoteUser = "vincent";
		bindPort = 2222;
	};
	networking.hostName = "kobe";

}
