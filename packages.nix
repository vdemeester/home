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
				cryptsetup
				direnv
				file
				gcc
				gptfdisk
				git
				gnumake
				gnupg
				haskellPackages.git-annex
				htop
				lsof
				msmtp
				offlineimap
				psmisc
				tmux
				tree
				vim
				wget
		];
	};
}
