{ config, pkgs, ... }:

{
	services = {
		xserver =  {
			layout = "fr";
			xkbOptions = "eurosign:e";
			inputClassSections = [
					''
					Identifier      "TypeMatrix"
					MatchIsKeyboard "on"
					MatchVendor     "TypeMatrix.com"
					MatchProduct    "USB Keyboard"
					Driver          "evdev"
					Option          "XbkModel"      "tm2030USB"
					Option          "XkbLayout"     "fr"
					Option          "XkbVariant"    "bepo"
					''
			];
		};
	};
}
