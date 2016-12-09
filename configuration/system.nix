
{ configs, pkgs, ...}:

{
	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;
	boot.kernelPackages = pkgs.linuxPackages_4_8;

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
