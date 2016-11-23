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
				rev = "f12c7d67a0187070c7656d7add056a69136824fa";
				sha256 = "0qsx5zkwmwaj7ds52j24k60ppd6fy578p34bpqd623i415x7k40p";
			}) {};
		in {
			inherit (pinnedPkgs) doctl dobi vndr docker;
		};
	};
}
