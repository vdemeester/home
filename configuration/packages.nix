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
				dobi
				doctl
				file
				gcc
				gptfdisk
				git
				gnumake
				gnupg
				haskellPackages.git-annex
				htop
				iotop
				lsof
				msmtp
				netcat
				offlineimap
				psmisc
				tmux
				tree
				vim
				wget
		];
	};
}
