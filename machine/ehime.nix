{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/custom-packages.nix
			../configuration/common.nix
			../profiles/desktop.nix
			../profiles/dev.nix
			../profiles/dev.python.nix
			../profiles/dockerization.nix
			../location/home.nix
			../profiles/avahi.nix
		];

	time.timeZone = "Europe/Paris";

	boot.loader.systemd-boot.enable = false;
	boot.loader.grub.device = "/dev/sda";
	boot.initrd.checkJournalingFS = false;

	boot.kernel.sysctl."vm.overcommit_memory" = "1";

	# Enable the OpenSSH daemon.
	services.openssh.enable = true;
	users.users.root.openssh.authorizedKeys.keys =
		with import ../ssh-keys.nix; [ wakasu ];
}
