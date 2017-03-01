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
				rev = "614afce3a88bf845e1116f0f2958b4c111ca238e";
				sha256 = "131qs7kfv5xr6maz6sjmziv90652bxn3hyap79nsxxij8irwv6vj";
			}) {};
			sbrPkgs = import (fetchNixPkgs {
				owner = "vdemeester";
				repo = "sbrpkgs";
				rev = "74e49a9213f52b44638845532e8bc01b9409833b";
				sha256 = "0zcwc7xhr2ml896miibafaa3arqrp40hxl3pf984ybls80hh0fy7";
			}) {};
		in {
			inherit (pinnedPkgs) keybase ipfs mpv docker-machine docker tini docker-proxy containerd runc doctl vndr emacs;
			inherit (sbrPkgs) dobi ape tuck clasp;
		};
	};
}
