{ config, lib, pkgs, ... }:

{
	nixpkgs.config = {
		packageOverrides = self: with self; let
			fetchNixPkgs = { rev, sha256, owner, repo }:
				fetchFromGitHub {
					inherit sha256 rev owner repo;
				};
			dockerPkgs = import (fetchNixPkgs {
				owner = "NixOS";
				repo = "nixpkgs";
				rev = "37f59d2e7284e275eafb627cbe4764dc0c30c79e";
				sha256 = "1n9w1w827wxxd9yimm879nawzdxp1fbb8007inqmj2rrz2p29qbl";
			}) {};
			# nixos-unstable
			unstablePkgs = import (fetchNixPkgs {
				owner = "NixOS";
				repo = "nixpkgs-channels";
				rev = "129f8d7e999b1a1f0fceaecadca30211e34d85a6";
				sha256 = "1sz6xm7xgyyp7an5gh4ck6lwgxil0rkgwg0f11awv13p67z9v763";
			}) {};
			sbrPkgs = import (fetchNixPkgs {
				owner = "vdemeester";
				repo = "sbrpkgs";
				rev = "faaef6ef32138cc79b39212d9aba4f20d0fecb60";
				sha256 = "191hxpyx71bzvbrzsil8nzadr09j1dma9s19cnh39hkdi1x87ih6";
			}) {};
		in {
			inherit (dockerPkgs) docker docker-edge docker-proxy containerd runc tini;
			inherit (unstablePkgs) keybase ipfs mpv docker-compose docker-machine doctl vndr emacs ledger-cli firefox youtube-dl go hasklig i3lock-color;
			inherit (sbrPkgs) dobi ape tuck clasp;
		};
	};
}
