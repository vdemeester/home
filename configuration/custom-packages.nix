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
				rev = "7f59fc657c1015bcefd39a5c957ec9b934a51419";
				sha256 = "01c0qzik6n07pxk0slqky5iy5cn7xz3qhh726vsiiahfqjkjq4cp";
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
