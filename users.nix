{ config, pkgs, ... }:

{
	users = {
		extraUsers = {
			vincent = {
				isNormalUser = true;
				uid = 1000;
				createHome = true;
				extraGroups = [ "networkmanager" "wheel" ];
				initialPassword = "changeMe";
			};
		};
	};
}
