{ config, pkgs, ... }:

{
	services.syncthing = {
		enable = true;
		useInotify = true;
		user = "vincent";
		dataDir = "/home/vincent/.syncthing";
		openDefaultPorts = true;
	};
}