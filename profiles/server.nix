# Common configuration for any server

{ configs, pkgs, ...}:

{
	imports = [
    ./default.nix
		./ssh.nix
	];
	boot.loader.efi.canTouchEfiVariables = true;
}
