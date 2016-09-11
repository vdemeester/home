{ config, pkgs, ... }:

{
	environment = {
		systemPackages = with pkgs; [
				wget
				git
				vim
				firefox
				emacs
				i3status
				dmenu
		];
	};
}
