{ config, pkgs, ... }:

{
	imports =
		[
			./docker.nix
		];
	virtualisation = {
		virtualbox.host.enable = true;
	};
	networking.firewall.trustedInterfaces = [ "docker0" "vboxnet0" ];
	environment = {
		systemPackages = with pkgs; [
			vagrant
		];
	};
}
