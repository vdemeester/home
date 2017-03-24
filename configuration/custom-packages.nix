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
				rev = "73a6832ad86755ae4dfb7717b4d1e37b2691fb4f";
				sha256 = "0dfjkm5gvy5v6dmfh47isfh73j39k3wzw4r1mhvgcm3620vhvzd9";
			}) {};
			sbrPkgs = import (fetchNixPkgs {
				owner = "vdemeester";
				repo = "sbrpkgs";
				rev = "73dc08ef1e4ec0fd5ae7bbadd7d4bdac2412d953";
				sha256 = "13g8l66gallqk3gp9ah481zsd569c1rf2ck4kgma652v6wgpxdmz";
			}) {};
		in {
			inherit (pinnedPkgs) keybase ipfs mpv docker-machine docker tini docker-proxy containerd runc doctl vndr emacs ledger-cli;
			inherit (sbrPkgs) dobi ape tuck clasp;
		};
	};
}
