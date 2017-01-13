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
				rev = "89dfe67f81addb0ffacbfa14079579c24c2a4530";
				sha256 = "1l8gz2k0w9grrkj30zsyk3k6jzcx52icysrli87xsz8yk5qpaj5i";
			}) {};
			sbrPkgs = import (fetchNixPkgs {
				owner = "vdemeester";
				repo = "sbrpkgs";
				rev = "162d2fd7c2ec7b8afff22299e3d025f1397dc738";
				sha256 = "10mawxggpdqmb9nzag1xk3afynrbmkwcy0xp42mndfjilrwgi3d1";
			}) {};
		in {
			inherit (pinnedPkgs) keybase ipfs mpv;
			inherit (sbrPkgs) dobi vndr docker ape tuck clasp;
		};
	};
}
