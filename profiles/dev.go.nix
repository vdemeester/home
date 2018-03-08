# Go(lang) configuration
{ configs, pkgs, ...}:

{
	nixpkgs.config = {
		packageOverrides = self: with self; let
			fetchNixPkgs = { rev, sha256, owner, repo }:
				fetchFromGitHub {
					inherit sha256 rev owner repo;
				};
      goPkgs = import (fetchNixPkgs {
      owner = "NixOS";
      repo = "nixpkgs-channels";
			rev = "cc4677c36ee8d880e881459ad114fd2224b3ac1c";
			sha256 = "1rc1pjnvfi194gka45zc1nivzsncc819kvxlfv277l2c8ryhgbpc";
    }) {};
		in {
			inherit (goPkgs) go_1_9 gotools golint godef gocode gotests gopkgs gomodifytags go-outline go-symbols goconvey delve vndr dep;
		};
	};
	imports = 
	[
		./dev.nix
	];
  environment.systemPackages = with pkgs; [
    go_1_9
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
