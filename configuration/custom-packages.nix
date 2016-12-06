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
				rev = "5476c54fe0702674a52b720bb7bf413f23ca79a7";
				sha256 = "103viqszk4h4xl8pq2smrqbp6fy1nv3w5yffbgv96h8dxd52g829";
			}) {};
		in {
			inherit (pinnedPkgs) doctl dobi vndr docker;
		};
	};
}
