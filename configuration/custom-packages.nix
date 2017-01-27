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
				rev = "ab6f3609e1c32141f3f2d24811234532fb99d5ab";
				sha256 = "1wh7hzg5b9x90zhi5sy3p76690srhg6r0p9hb1hx6jq8lmslfqgh";
			}) {};
			sbrPkgs = import (fetchNixPkgs {
				owner = "vdemeester";
				repo = "sbrpkgs";
				rev = "4e0d1dd6ac0555a5086e9a312de21ca2506dc28b";
				sha256 = "0hm42619239vhk2nzlprcibv0pc93vyldwn47a00i5hv581f25rg";
			}) {};
		in {
			inherit (pinnedPkgs) keybase ipfs mpv docker-machine docker tini docker-proxy containerd runc;
			inherit (sbrPkgs) dobi vndr ape tuck clasp;
		};
	};
}
