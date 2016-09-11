{ config, pkgs, ... }:

{
	programs = {
		 zsh.enable = true;
	};
	environment = {
		systemPackages = with pkgs; [
				wget
				git
				vim
				tmux
				htop
		];
	};
}
