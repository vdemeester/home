{ config, pkgs, ... }:

{
	imports =[
		<nixpkgs/nixos/modules/services/hardware/sane_extra_backends/brscan4.nix>
	];

	environment.systemPackages = with pkgs; [
		zoom-us
	];

	hardware.sane = {
		brscan4.enable = true;
		brscan4.netDevices = {
      			docker = { model = "MFC-9330CDW"; ip = "10.0.0.28"; };
    		};
	};
}
