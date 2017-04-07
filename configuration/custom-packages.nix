{ config, lib, pkgs, ... }:

{
	nixpkgs.config = {
		packageOverrides = self: with self; let
			fetchNixPkgs = { rev, sha256, owner, repo }:
				fetchFromGitHub {
					inherit sha256 rev owner repo;
				};
			pinnedPkgs = import (fetchNixPkgs {
				owner = "NixOS";
				repo = "nixpkgs-channels";
				rev = "0a6a06346a3c928a3115dfd700993c5c06b0a1e9";
				sha256 = "1gk9phpqj9nchkgv22blq8ggwdfyqsr3i55vwpjd8m4qcr5x62zw";
			}) {};
			sbrPkgs = import (fetchNixPkgs {
				owner = "vdemeester";
				repo = "sbrpkgs";
				rev = "73dc08ef1e4ec0fd5ae7bbadd7d4bdac2412d953";
				sha256 = "13g8l66gallqk3gp9ah481zsd569c1rf2ck4kgma652v6wgpxdmz";
			}) {};
		in {
			inherit (pinnedPkgs) keybase ipfs mpv docker-machine docker docker-edge tini docker-proxy containerd runc doctl vndr emacs ledger-cli;
			inherit (sbrPkgs) dobi ape tuck clasp;
		};
	};
}
