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
				rev = "a6dca0427221d7c249a9b6f1581cf0d73baf51da";
				sha256 = "15fcl29a97f68j1pjywmrjm31rdh1a21jz9airlsbzpl4lc3zhfi";
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
