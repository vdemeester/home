{ config, pkgs, ... }:

{
	environment = {
		systemPackages = with pkgs; [
				aspell
				aspellDicts.en
				aspellDicts.fr
				cryptsetup
				direnv
				#dobi
				doctl
				gcc
				gnumake
				gnupg
				gptfdisk
				haskellPackages.git-annex
				msmtp
				offlineimap
		];
	};
}
