# Go(lang) configuration
{ configs, pkgs, ...}:

{
	imports = [
		./dev.nix
	];
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
    protobuf
  ];
}
