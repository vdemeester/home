{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../profiles/server.nix
			../profiles/gitconfig.nix
			../profiles/dev.nix
			../profiles/avahi.nix
			../profiles/syncthing.nix
			../service/ssh-tunnel.nix
      ../location/home.nix
		];

	time.timeZone = "Europe/Paris";
	boot.loader.systemd-boot.enable = true;

	services.openssh.enable = true;

	networking.enableIPv6 = false;
	networking.firewall.allowedTCPPorts = [ 80 443 2375 8384 ];

  home-manager.users.vincent = {...}: {
    imports = [ ../envs/hokkaido.nix ../envs/base.nix ];
  };

	virtualisation.docker = {
		enable = true;
		package = pkgs.docker-edge;
		storageDriver = "overlay2";
		extraOptions = "--experimental --host=tcp://0.0.0.0:2375";
	};
	services.ssh-tunnel = {
		enable = true;
		localUser = "vincent";
		remoteHostname = "95.85.58.158";
		remotePort = 22;
		remoteUser = "vincent";
		bindPort = 2223;
	};

	fileSystems."/data/nyan" = {
		device = "/dev/disk/by-uuid/9d4ee0b8-bffb-4a18-8c23-cca3bf5b4487";
		fsType = "ext4";
		options = ["relatime"];
	};
	fileSystems."/data/toshito" = {
		device = "/dev/disk/by-uuid/57b0853e-6650-41d6-b42a-93f2fd182d2a";
		fsType = "ext4";
		options = ["relatime"];
	};
}
