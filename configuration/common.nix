{ configs, pkgs, ...}:

# This is the common configuration shared by any of my machine
# You should include it in `configuration.nix`.

{
	imports =
		[
			# ./security.nix
			./users.nix
			./packages.nix
		];
}
