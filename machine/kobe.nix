{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/custom-packages.nix
			../configuration/users.nix
			../profiles/server.nix
			../profiles/dockerization.nix
			../profiles/avahi.nix
			../location/docker.nix
			../service/ssh-tunnel.nix
		];

	time.timeZone = "Europe/Paris";

	# Enable the OpenSSH daemon.
	services.openssh.enable = true;
	users.users.root.openssh.authorizedKeys.keys =
		with import ../ssh-keys.nix; [ wakasu hokkaido ];

	virtualisation.docker.extraOptions = "--label=type=server -H unix:///var/run/docker.sock -H 0.0.0.0:2375";
	networking.firewall.allowedTCPPorts = [ 2375 ];

	# remote reverse ssh tunnel
	services.ssh-tunnel = {
		enable = true;
		localUser = "vincent";
		remoteHostname = "95.85.58.158";
		remotePort = 22;
		remoteUser = "vincent";
		bindPort = 2222;
	};

	# TODO Move this in a profile
	environment.systemPackages = with pkgs; [
		ape
	];
	# ape – sync git mirrors
	systemd.user.services.ape = {
		description = "Ape - sync git mirrors";
		wantedBy = [ "multi-user.target" ];
		serviceConfig = {
			Type = "oneshot";
			ExecStart = "${pkgs.ape}/bin/ape up /home/vincent/mirrors/";
			Environment = "PATH=/run/current-system/sw/bin/";
		};
	};
	systemd.user.timers.ape = {
		description = "Ape hourly";
		wantedBy = [ "timers.target" ];
		timerConfig = {
			OnCalendar = "hourly";
			Persistent = "true";
		};
	};
	systemd.user.timers.ape.enable = true;
}
