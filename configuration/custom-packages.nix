{ config, lib, pkgs, ... }:

{
	nixpkgs.config = {
		packageOverrides = self: with self; let
			fetchNixPkgs = { rev, sha256, owner, repo }:
				fetchFromGitHub {
					inherit sha256 rev owner repo;
				};
			# nixos-unstable
			dockerPkgs = import (fetchNixPkgs {
				owner = "vdemeester";
				repo = "nixpkgs";
				rev = "398f6ed7d3b385cf11d95f4d83f5012300112444";
				sha256 = "0sag7j6p26f04sbvz8i5naa0nibw9iczbjhw42zz7i9aj0aa0cq0";
			}) {};
			pinnedPkgs = import (fetchNixPkgs {
				owner = "NixOS";
				repo = "nixpkgs-channels";
				rev = "c90998d5cf8b8d7983f5f547546ee9ef2ad11688";
				sha256 = "0n4mcpanicgwfmvsay3nccwf59b1v8g91pn96797xy7cq8l9v1zk";
			}) {};
			sbrPkgs = import (fetchNixPkgs {
				owner = "vdemeester";
				repo = "sbrpkgs";
				rev = "73dc08ef1e4ec0fd5ae7bbadd7d4bdac2412d953";
				sha256 = "13g8l66gallqk3gp9ah481zsd569c1rf2ck4kgma652v6wgpxdmz";
			}) {};
		in {
			inherit (dockerPkgs) docker docker-edge docker-proxy containerd runc tini;
			inherit (pinnedPkgs) keybase ipfs mpv docker-machine doctl vndr emacs ledger-cli;
			inherit (sbrPkgs) dobi ape tuck clasp;
		};
	};
}
