{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../hardware/dell-latitude-e6540.nix
			../profiles/server.nix
			../profiles/dev.python.nix
			../profiles/dev.go.nix
			../location/home.nix
			../service/ssh-tunnel.nix
		];

	time.timeZone = "Europe/Paris";

  home-manager.users.vincent = {...}: {
    imports = [ ../envs/honshu.nix ../envs/server.nix ];
  };
  
  virtualisation.docker = {
		enable = true;
		package = pkgs.docker-edge;
		storageDriver = "overlay2";
		extraOptions = "--experimental --host=tcp://0.0.0.0:2375";
	};
	services = {
    ssh-tunnel = {
			enable = true;
			localUser = "vincent";
			remoteHostname = "95.85.58.158";
			remotePort = 22;
			remoteUser = "vincent";
			bindPort = 2224;
		};
	};
  
	networking.enableIPv6 = false;
	networking.firewall.allowedTCPPorts = [ 3389 2375 ];
}
