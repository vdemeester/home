{ config, pkgs, ... }:

{
	users = {
		extraUsers = {
			vincent = {
				isNormalUser = true;
				uid = 1000;
				createHome = true;
				extraGroups = [ "networkmanager" "wheel" ];
				shell = "/run/current-system/sw/bin/zsh";
				initialPassword = "changeMe";
			};
		};
	};
}
