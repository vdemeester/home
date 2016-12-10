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
			./audio.nix
			./gui.nix
			./virtualisation.nix
			./security.nix
			./users.nix
			./packages.nix
		];
	i18n = {
		consoleFont = "Lat2-Terminus16";
		consoleKeyMap = "fr";
		defaultLocale = "en_US.UTF-8";
		# supportedLocales = [ "en_US/ISO-8859-1" "fr_FR/ISO-8859-1" "fr_FR@euro/ISO-8859-15" ];
	};
}
