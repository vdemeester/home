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
				haskellPackages.git-annex
				vim
				tmux
				htop
				tree
				lsof
				psmisc
				aspell
				aspellDicts.en
				aspellDicts.fr
		];
	};
        
}
