{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/custom-packages.nix
			../configuration/users.nix
			../profiles/dev.nix
			../profiles/avahi.nix
		];

	time.timeZone = "Europe/Paris";

	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;

	boot.initrd.kernelModules = ["hv_vmbus" "hv_storvsc"];
	boot.initrd.checkJournalingFS = false;

	boot.kernel.sysctl."vm.overcommit_memory" = "1";

	services.xserver = {
		enable = true;
		layout = "fr";

		desktopManager = {
			gnome3.enable = true;
			default = "gnome3";
		};
	};

	# Enable the OpenSSH daemon.
	services.openssh.enable = true;
	users.users.root.openssh.authorizedKeys.keys =
		with import ../ssh-keys.nix; [ wakasu ];
}
