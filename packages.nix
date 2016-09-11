{ config, pkgs, ... }:

{
	programs = {
		 zsh.enable = true;
	};
	environment = {
		variables = {
			EDITOR = pkgs.lib.mkOverride 0 "vim";
		};
		systemPackages = with pkgs; [
				wget
				git
				vim
				tmux
				htop
		];
	};
}
