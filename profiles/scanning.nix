{ config, pkgs, ... }:

{
	environment.systemPackages = with pkgs; [
		saneFrontends
		simple-scan
	];
	hardware.sane = {
		enable = true;
	};
}
