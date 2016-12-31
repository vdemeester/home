{ config, lib, pkgs, ... }:

{
	nixpkgs.config = {
		packageOverrides = self: with self; let
			fetchNixPkgs = { rev, sha256 }:
				fetchFromGitHub {
					inherit sha256 rev;
					owner = "vdemeester";
					repo = "sbrpkgs";
				};
			pinnedPkgs = import (fetchNixPkgs {
				rev = "19cb2f23b1bec2538ed3ae14cfae6218bb4dddaa";
				sha256 = "13j5cdlqgabr6mchymx77anqxnldxpkdxgq9kk7p911k5fg0vpq1";
			}) {};
		in {
			inherit (pinnedPkgs) dobi vndr docker;
		};
	};
}
