{ config, pkgs, ... }:

{
	programs = {
		 zsh.enable = true;
	};
	users = {
		extraUsers = {
			vincent = {
				isNormalUser = true;
				uid = 1000;
				createHome = true;
				extraGroups = [ "networkmanager" "wheel" "docker" "vboxusers" "input" "audio" "video" ];
				shell = "/run/current-system/sw/bin/zsh";
				initialPassword = "changeMe";
			};
		};
	};
}
