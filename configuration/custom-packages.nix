{ config, lib, pkgs, ... }:

{
	nixpkgs.config = {
		packageOverrides = self: with self; let
			fetchNixPkgs = { rev, sha256, owner, repo }:
				fetchFromGitHub {
					inherit sha256 rev owner repo;
				};
			vdePinnedPkgs = import (fetchNixPkgs {
				owner = "vdemeester";
				repo = "nixpkgs";
				rev = "74d4d3e4f9bec56e20bb0adcb6dd8df6c4d14247";
				sha256 = "02ll11ncnal33vxiayv2kdwvlwfyllb8rgas2vbiv8b8gihy9c6x";
			}) {};
			pinnedPkgs = import (fetchNixPkgs {
				owner = "NixOS";
				repo = "nixpkgs";
				rev = "89dfe67f81addb0ffacbfa14079579c24c2a4530";
				sha256 = "1l8gz2k0w9grrkj30zsyk3k6jzcx52icysrli87xsz8yk5qpaj5i";
			}) {};
			sbrPkgs = import (fetchNixPkgs {
				owner = "vdemeester";
				repo = "sbrpkgs";
				rev = "4e0d1dd6ac0555a5086e9a312de21ca2506dc28b";
				sha256 = "0hm42619239vhk2nzlprcibv0pc93vyldwn47a00i5hv581f25rg";
			}) {};
		in {
			inherit (pinnedPkgs) keybase ipfs mpv docker-machine;
			inherit (sbrPkgs) dobi vndr ape tuck clasp;
			inherit (vdePinnedPkgs) docker;
		};
	};
}
