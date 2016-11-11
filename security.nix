{ config, pkgs, ... }:

{
        security = {
	        pam.enableU2F = true;
		pam.services.vincent.u2fAuth = true;
		sudo.enable = true;
		# sudo.wheelNeedsPassworld = true;
	};

	services.pcscd.enable = true;
	
	#services.udev.extraRules = ''
    # Yubico YubiKey
    #KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", TAG+="uaccess"
   #ACTION=="remove", ENV{ID_VENDOR_ID}=="1050", ENV{ID_MODEL_ID}=="0113|0114|0115|0116|0120|0402|0403|0406|0407|0410", RUN+="/run/current-system/sw/bin/loginctl lock-sessions"
#'';
	environment = {
		systemPackages = with pkgs; [
				yubico-piv-tool
		];
	};
}
