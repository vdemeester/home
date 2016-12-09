{ config, pkgs, ... }:

{
	services.openssh.enable = true;
	services.openssh.startWhenNeeded = true;
}
