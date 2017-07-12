# Common configuration for any machine to do dev

{ configs, pkgs, ...}:

{
	environment.systemPackages = with pkgs; [
		jq
		grc
		dobi
		platinum-searcher
		ripgrep
	];
}
