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
				repo = "nixpkgs";
				rev = "557fd03926e174e20e3f6153fc79a652cc1452d6";
				sha256 = "1vibkh19acn1yjx31c5fajaj5lyxx4gsb5i7c6iwbc5zzhbh5lvm";
			}) {};
			sbrPkgs = import (fetchNixPkgs {
				owner = "vdemeester";
				repo = "sbrpkgs";
				rev = "4e0d1dd6ac0555a5086e9a312de21ca2506dc28b";
				sha256 = "0hm42619239vhk2nzlprcibv0pc93vyldwn47a00i5hv581f25rg";
			}) {};
		in {
			inherit (pinnedPkgs) keybase ipfs mpv docker-machine docker tini docker-proxy containerd runc doctl vndr;
			inherit (sbrPkgs) dobi ape tuck clasp;
		};
	};
}
