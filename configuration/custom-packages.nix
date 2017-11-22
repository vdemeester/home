{ config, lib, pkgs, ... }:

{
	nixpkgs.config = {
		packageOverrides = self: with self; let
			fetchNixPkgs = { rev, sha256, owner, repo }:
				fetchFromGitHub {
					inherit sha256 rev owner repo;
				};
			masterUnstablePkgs = import (fetchNixPkgs {
				owner = "NixOS";
				repo = "nixpkgs";
				rev = "91054336ce8fcaedb5d98a4b53f177b52f63d0d6";
				sha256 = "0wq05wgkpjipxrdiixrk1grhbalvszw7pd63s6f40m14z3kk6df2";
			}) {};
			# nixos-unstable
			unstablePkgs = import (fetchNixPkgs {
				owner = "NixOS";
				repo = "nixpkgs-channels";
				rev = "799435b7cab97a39893a104999b3bc589e1172b1";
				sha256 = "1x61hpkagydrf05y0sa1ynmi8z3sm2377f4f6yiqlj9yvkg57jv3";
			}) {};
			sbrPkgs = import (fetchNixPkgs {
				owner = "vdemeester";
				repo = "sbrpkgs";
				rev = "df281994c5e438c25af6c054ebfbd19333f3e132";
				sha256 = "0636k102vw1pmbcch75xvhjlkfk9553bcf6rba5i69m7b5bdsfd0";
			}) {};
		in {
			inherit (masterUnstablePkgs) docker docker-edge docker-proxy containerd runc tini;
			inherit (unstablePkgs) keybase ipfs mpv docker-compose docker-machine doctl vndr emacs ledger-cli youtube-dl go hasklig i3lock-color certstrap;
			#inherit (unstablePkgs) keybase ipfs mpv docker-compose docker-machine doctl vndr emacs ledger-cli firefox google-chrome-stable youtube-dl go hasklig i3lock-color certstrap;
			inherit (sbrPkgs) dobi ape tuck clasp;
		};
	};
}
