# Go(lang) configuration
{ configs, pkgs, ...}:

{
	imports = 
	[
		./dev.nix
	];
	nixpkgs.config = {
		packageOverrides = self: with self; let
			fetchNixPkgs = { rev, sha256, owner, repo }:
				fetchFromGitHub {
					inherit sha256 rev owner repo;
				};
      goPkgs = import (fetchNixPkgs {
      owner = "NixOS";
      repo = "nixpkgs-channels";
			rev = "0e7c9b32817e5cbe61212d47a6cf9bcd71789322";
			sha256 = "1dm777cmlhqcwlrq8zl9q2d87h3p70rclpvq36y43kp378f3pd0y";
    }) {};
		in {
			inherit (goPkgs) go gotools golint godef gocode gotests gopkgs gomodifytags go-outline go-symbols goconvey delve vndr dep;
		};
	};
  environment.systemPackages = with pkgs; [
    go
    gcc
    # tools
    gotools
    golint
    godef
    gocode
    gotests
    gopkgs
    gomodifytags
    go-outline
    go-symbols
    goconvey
    delve
    # vendoring tools
    vndr
    dep
    # misc
    protobuf3_3
  ];
}
