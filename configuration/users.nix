{ config, pkgs, ... }:

{
	users = {
		extraUsers = {
			vincent = {
				isNormalUser = true;
				uid = 1000;
				createHome = true;
				extraGroups = [ "networkmanager" "wheel" "docker" "vboxusers" "input" "audio" "video" "scanner" ];
				shell = "/run/current-system/sw/bin/zsh";
				initialPassword = "changeMe";
				openssh.authorizedKeys.keys =
					with import ../ssh-keys.nix; [ honshu wakasu kobe ];
			};
		};
	};
}
