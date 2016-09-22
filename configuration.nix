{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			./hardware-configuration.nix
			./keyboard.nix
                        ./acpi.nix
			./network.nix
			./audio.nix
			./gui.nix
			./virtualisation.nix
			./security.nix
			./users.nix
			./packages.nix
			./local-configuration.nix
		];

	boot.loader.gummiboot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;

	i18n = {
		consoleFont = "Lat2-Terminus16";
		consoleKeyMap = "fr";
		defaultLocale = "en_US.UTF-8";
	};

	time.timeZone = "Europe/Paris";

	system = {
		stateVersion = "16.03";	
		autoUpgrade = {
			enable = true;
			dates = "13:00";
		};
	};

	nixpkgs = {
		config = {
			allowUnfree = true;
		};
	};
}
