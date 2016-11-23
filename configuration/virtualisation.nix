{ config, pkgs, ... }:

{
	virtualisation = {
		docker = {
			enable = true;
			# experimental = true;
			socketActivation = false;
			storageDriver = "overlay2";
			extraOptions = "--label=type=desktop";
		};
		virtualbox.host.enable = true;
	};
	networking.firewall.trustedInterfaces = [ "docker0" "vboxnet0" ];
	environment = {
		systemPackages = with pkgs; [
			vagrant
		];
	};
}
