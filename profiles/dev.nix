# Common configuration for any machine to do dev

{ configs, pkgs, ...}:

{
	imports = [
		./gitconfig.nix
	];
	environment.systemPackages = with pkgs; [
		git
		grc
		ripgrep
    gnumake
	];
}
