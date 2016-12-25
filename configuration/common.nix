{ configs, pkgs, ...}:

# This is the common configuration shared by any of my machine
# You should include it in `configuration.nix`.

{
	imports =
		[
			./system.nix
			./keyboard.nix
                        ./acpi.nix
			./network.nix
			./network-gui.nix
			./gui.nix
			./virtualisation.nix
			./security.nix
			./users.nix
			./packages.nix
		];
}
