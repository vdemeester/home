# Common configuration for any machine to do dev

{ configs, pkgs, ...}:

{
	environment.systemPackages = with pkgs; [
		go
		jq
		grc
		dobi
	];
}
