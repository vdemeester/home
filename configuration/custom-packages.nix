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
			unstablePkgs = import (fetchNixPkgs {
				owner = "NixOS";
				repo = "nixpkgs-channels";
				rev = "0afb6d789c8bf74825e8cdf6a5d3b9ab8bde4f2d";
				sha256 = "147vhzrnwcy0v77kgbap31698qbda8rn09n5fnjp740svmkjpaiz";
			}) {};
			sbrPkgs = import (fetchNixPkgs {
				owner = "vdemeester";
				repo = "sbrpkgs";
				rev = "62699b6f1233b97aa79227e391f16531eecabf6b";
				sha256 = "02zklicpvn54xlicbxh20bbcxxpmh5kr1s06rdr3wxagfyxhmw0g";
			}) {};
		in {
			inherit (dockerPkgs) docker docker-edge docker-proxy containerd runc tini;
			inherit (unstablePkgs) keybase ipfs mpv docker-machine doctl vndr emacs ledger-cli firefox;
			inherit (sbrPkgs) dobi ape tuck clasp;
		};
	};
}
