{ config, lib, pkgs, ... }:

{
	nixpkgs.config = {
		packageOverrides = self: with self; let
			fetchNixPkgs = { rev, sha256, owner, repo }:
				fetchFromGitHub {
					inherit sha256 rev owner repo;
				};
			dockerUnstablePkgs = import (fetchNixPkgs {
				owner = "NixOS";
				repo = "nixpkgs-channels";
				rev = "8ecadc12502d59fc8117ca0ed41ede010448fca4";
				sha256 = "102wvwixvnbkr547ay6psvv1x31001mb5y17ibkplyikb91xi2ak";
			}) {};
			# nixos-unstable
			unstablePkgs = import (fetchNixPkgs {
				owner = "NixOS";
				repo = "nixpkgs-channels";
				rev = "9c048f4fb66adc33c6b379f2edefcb615fd53de6";
				sha256 = "18xbnfzj753bphzmgp74rn9is4n5ir4mvb4gp9lgpqrbfyy5dl2j";
			}) {};
			sbrPkgs = import (fetchNixPkgs {
				owner = "vdemeester";
				repo = "sbrpkgs";
				rev = "df281994c5e438c25af6c054ebfbd19333f3e132";
				sha256 = "0636k102vw1pmbcch75xvhjlkfk9553bcf6rba5i69m7b5bdsfd0";
			}) {};
		in {
			inherit (dockerUnstablePkgs) docker docker-edge docker-proxy containerd runc tini docker-compose docker-machine;
			inherit (unstablePkgs) keybase ipfs mpv doctl vndr emacs ledger-cli youtube-dl go hasklig i3lock-color certstrap pipenv;
			inherit (sbrPkgs) dobi ape tuck clasp;
		};
	};
}
