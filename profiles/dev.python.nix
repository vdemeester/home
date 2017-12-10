# Configuration for python dev machine

{ configs, pkgs, ...}:

{
	environment.systemPackages = with pkgs; [
		python3
		pipenv
	];
}