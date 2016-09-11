{ config, pkgs, ... }:

{
	environment = {
		systemPackages = with pkgs; [
				wget
				firefox
				git
				vim
		];
	};
}
