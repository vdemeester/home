# Configuration for python dev machine

{ configs, pkgs, ...}:

{
	imports = 
		[
			./dev.nix
		];
	environment.systemPackages = with pkgs; [
		python3
		pipenv
	];
}
