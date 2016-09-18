{ config, pkgs, ... }:

{
	virtualisation = {
		docker = {
			enable = true;
			socketActivation = false;
			# storageDriver = "overlay2"; # use it when docker 1.12 is here
			storageDriver = "overlay";
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
