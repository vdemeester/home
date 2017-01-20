# Common configuration for any desktop (not that laptop are a superset of desktop)

{ configs, pkgs, ...}:

{
	imports = [
		./printing.nix
		./scanning.nix
		./avahi.nix
	];

	time.timeZone = "Europe/Paris";

	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;
	boot.kernelPackages = pkgs.linuxPackages_4_8;
	boot.tmpOnTmpfs = true;

	environment.systemPackages = with pkgs; [
		dmenu2
		dunst
		emacs
		firefox
		gnome3.defaultIconTheme
		gnome3.gnome_themes_standard
		# adapta-gtk-theme # wait for 16.09 on this one
		i3status
		i3lock
		libnotify
		pythonPackages.udiskie
		scrot
		termite
		xdg-user-dirs
		xdg_utils
		xlibs.xmodmap
		xorg.xbacklight
		xss-lock
		ape
		tuck
		clasp
		keybase
		ipfs
		mpv
	];
	hardware.opengl.extraPackages = [ pkgs.vaapiIntel ];
	services = {
		xserver = {
			enable = true;
			enableTCP = false;
			libinput.enable = true;
			layout = "fr";
			xkbVariant = "oss";
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
					''
Identifier      "ErgoDox"
MatchIsKeyboard "on"
#MatchVendor     "ErgoDox_EZ"
#MatchProduct    "ErgoDox_EZ"
MatchUSBID      "feed:1307"
Driver          "evdev"
Option          "XkbLayout"     "fr"
Option          "XkbVariant"    "bepo"
					''
					''
Identifier "evdev touchpad off"
MatchIsTouchpad "on"
MatchDevicePath "/dev/input/event*"
Driver "evdev"
Option "Ignore" "true"
					''
			];
			windowManager = {
				i3 = {
					enable = true;
				};
				default = "i3";
			};
			displayManager = {
				slim = {
					enable = true;
					# Probably put this into users instead ?
					defaultUser = "vincent";
				};
				sessionCommands = ''
${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.Xmodmap &
${pkgs.pythonPackages.udiskie}/bin/udiskie -a -t -n -F &
				'';
			};
			xkbOptions = "compose:caps";
		};
		unclutter.enable = true;
		redshift = {
			enable = true;
			brightness.day = "0.95";
			brightness.night = "0.7";
			latitude = "48.3";
			longitude = "7.5";
		};
	};
	fonts = {
		enableFontDir = true;
		enableGhostscriptFonts = true;
		fonts = with pkgs; [
			corefonts
			inconsolata
			dejavu_fonts
			ubuntu_font_family
			unifont
			# google-fonts
			emojione
			symbola
			fira
			fira-code
			fira-mono
			font-droid
		];
	};
}
