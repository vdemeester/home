{ config, pkgs, ... }:

{
	imports = [
		./synergy.nix
	];
	systemd.user.services.synergy = {
		description = "Synergy sharing server";
		wantedBy = [ "multi-user.target" ];
		serviceConfig = {
			RestartSec = 10;
			Restart = "on-failure";
			ExecStart = "${pkgs.synergy}/bin/synergys -c /etc/synergy-server.conf -f -a 0.0.0.0";
			Environment = "PATH=/run/current-system/sw/bin/";
		};
	};
	systemd.user.services.synergy.enable = true;
	
	networking.firewall.allowedTCPPorts = [ 24800 ];
}
