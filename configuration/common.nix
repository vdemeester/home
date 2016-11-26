{ configs, pkgs, ...}:

# This is the common configuration shared by any of my machine
# You should include it in `configuration.nix`.

{
	imports =
		[
			./keyboard.nix
                        ./acpi.nix
			./network.nix
			./audio.nix
			./gui.nix
			./virtualisation.nix
			./security.nix
			./users.nix
			./packages.nix
		];
	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;
	boot.kernelPackages = pkgs.linuxPackages_4_8;
	i18n = {
		consoleFont = "Lat2-Terminus16";
		consoleKeyMap = "fr";
		defaultLocale = "en_US.UTF-8";
		supportedLocales = [ "en_US/ISO-8859-1" "fr_FR/ISO-8859-1" "fr_FR@euro/ISO-8859-15" ];
	};

	time.timeZone = "Europe/Paris";

	system = {
		stateVersion = "16.09";	
		autoUpgrade = {
			enable = true;
			dates = "13:00";
		};
	};

	nix = {
		useSandbox = true;
		# if hydra is down, don't wait forever
		extraOptions = ''
		    connect-timeout = 20
		'';
	};
	nixpkgs = {
		config = {
			allowUnfree = true;
		};
	};
}
