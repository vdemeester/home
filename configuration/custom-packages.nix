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
				rev = "f53900d142744344d31cc7fa40fc0e2db9c97ba6";
				sha256 = "05j997n5vagqnr1zvgv65z9p2d4r2qxxfg8q87ycq36ax20gc7iz";
			}) {};
			sbrPkgs = import (fetchNixPkgs {
				owner = "vdemeester";
				repo = "sbrpkgs";
				rev = "ba773ea628067f3069b918240704fe695ab2e55a";
				sha256 = "049h37lm82i96wkrwirnfnxmfywd3rq281g35znshkj0lqwayahz";
			}) {};
		in {
			inherit (pinnedPkgs) keybase ipfs mpv docker-machine docker tini docker-proxy containerd runc doctl vndr;
			inherit (sbrPkgs) dobi ape tuck clasp;
		};
	};
}
