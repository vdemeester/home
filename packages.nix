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
				aspell
				aspellDicts.en
				aspellDicts.fr
				direnv
				gptfdisk
				git
				gnumake
				gnupg
				haskellPackages.git-annex
				htop
				lsof
				psmisc
				tmux
				tree
				vim
				wget
		];
	};
}
