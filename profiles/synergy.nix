{ config, pkgs, ... }:

{
	environment = {
		systemPackages = with pkgs; [
			synergy
		];
	};
}
