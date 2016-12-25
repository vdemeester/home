{ config, pkgs, ... }:

{
	imports =[
		<nixpkgs/nixos/modules/services/hardware/sane_extra_backends/brscan4.nix>
	];

	hardware.sane = {
		brscan4.enable = true;
		brscan4.netDevices = {
      			docker = { model = "MFC-9330CDW"; ip = "192.168.1.57"; };
    		};
	};
}
