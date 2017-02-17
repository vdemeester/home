{ config, pkgs, ... }:

{
	imports =
		[ # Include the results of the hardware scan.
			../hardware-configuration.nix
			../configuration/custom-packages.nix
			../configuration/common.nix
			../profiles/ssh.nix
			../profiles/laptop.nix
			../profiles/virtualization.nix
			../profiles/dockerization.nix
			../profiles/office.nix
			../location/home.nix
			#../hardware/dell-latitude-e6540.nix
			../service/ssh-tunnel.nix
		];

#	boot = {
#		loader = {
#			grub = {
#				enable = true;
#				version = 2;
#				device = "/dev/sdc";
#				extraEntries = ''
#				menuentry "Windows 10" {
#					insmod ntfs
#					search --no-floppy --fs-uuid --set EE62F055662F023CD
#					chainloarder +1
#				}
#				'';
#			};
#		};
#	};
	
}
