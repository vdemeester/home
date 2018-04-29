{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../hardware/dell-latitude-e6540.nix
			../profiles/server.nix
			../profiles/dev.python.nix
			../profiles/dev.go.nix
			../profiles/dockerization.nix
			../location/home.nix
			../service/ssh-tunnel.nix
		];

	time.timeZone = "Europe/Paris";
	boot.loader.systemd-boot.enable = true;

  home-manager.users.vincent = {...}: {
    imports = [ ../envs/honshu.nix ../envs/server.nix ];
  };

	services = {
		openssh = {
			enable = true;
		};
		ssh-tunnel = {
			enable = true;
			localUser = "vincent";
			remoteHostname = "95.85.58.158";
			remotePort = 22;
			remoteUser = "vincent";
			bindPort = 2224;
		};
	};
	networking.firewall.allowedTCPPorts = [ 3389 2375 ];
}
