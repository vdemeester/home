{ config, pkgs, ... }:

{
	i18n = {
		consoleFont = "Lat2-Terminus16";
		consoleKeyMap = "fr";
		defaultLocale = "en_US.UTF-8";
		# supportedLocales = [ "en_US/ISO-8859-1" "fr_FR/ISO-8859-1" "fr_FR@euro/ISO-8859-15" ];
	};
}
