{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../profiles/desktop.nix
			../profiles/ssh.nix
			../profiles/virtualization.nix
			../profiles/dockerization.nix
		];

	boot = {
		loader = {
			grub = {
				enable = true;
				device = "/dev/sda";
			};
		};
		initrd = {
			checkJournalingFS = false;
		};
	};

	virtualisation.virtualbox.guest.enable = true;

	services.xserver.xkbVariant = "bepo";
}
