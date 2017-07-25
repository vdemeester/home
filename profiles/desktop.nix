# Common configuration for any desktop (not that laptop are a superset of desktop)

{ configs, pkgs, ...}:

{
	imports = [
		./printing.nix
		./scanning.nix
		./avahi.nix
	];

	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;
	boot.kernelPackages = pkgs.linuxPackages_4_11;
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
		rofi
		libnotify
		pythonPackages.udiskie
		scrot
		termite
		xdg-user-dirs
		xdg_utils
		xlibs.xmodmap
		xorg.xbacklight
		xorg.xdpyinfo
		xorg.xhost
		xorg.xinit
		xss-lock
		xorg.xmessage
		ape
		tuck
		clasp
		keybase
		# ipfs # something is failing on 17.03
		mpv
		ledger
		offlineimap
		notmuch
		pythonPackages.afew
		unzip
	];
	hardware.opengl.extraPackages = [ pkgs.vaapiIntel ];
	services = {
		xserver = {
			enable = true;
			enableTCP = false;
			libinput.enable = true;
			synaptics.enable = false;
			layout = "fr(bepo),fr";
			xkbVariant = "oss";
			xkbOptions = "grp:menu_toggle,grp_led:caps,compose:caps";
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
#					''
#Identifier "evdev touchpad off"
#MatchIsTouchpad "on"
#MatchDevicePath "/dev/input/event*"
#Driver "evdev"
#Option "Ignore" "true"
#					''
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
		};
		# unclutter.enable = true;
		#redshift = {
		#	enable = true;
		#	brightness.day = "0.95";
		#	brightness.night = "0.7";
		#	latitude = "48.3";
		#	longitude = "7.5";
		#};
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
			emojione
			symbola
			fira
			fira-code
			fira-mono
			font-droid
			hasklig
		];
	};

	# Polkit.
	security.polkit.extraConfig = ''
	polkit.addRule(function(action, subject) {
		if ((action.id == "org.freedesktop.udisks2.filesystem-mount-system" ||
		action.id == "org.freedesktop.udisks2.encrypted-unlock-system"
		) &&
		subject.local && subject.active && subject.isInGroup("users")) {
			return polkit.Result.YES;
		}
		var YES = polkit.Result.YES;
		var permission = {
			// required for udisks1:
			"org.freedesktop.udisks.filesystem-mount": YES,
			"org.freedesktop.udisks.luks-unlock": YES,
			"org.freedesktop.udisks.drive-eject": YES,
			"org.freedesktop.udisks.drive-detach": YES,
			// required for udisks2:
			"org.freedesktop.udisks2.filesystem-mount": YES,
			"org.freedesktop.udisks2.encrypted-unlock": YES,
			"org.freedesktop.udisks2.eject-media": YES,
			"org.freedesktop.udisks2.power-off-drive": YES,
			// required for udisks2 if using udiskie from another seat (e.g. systemd):
			"org.freedesktop.udisks2.filesystem-mount-other-seat": YES,
			"org.freedesktop.udisks2.filesystem-unmount-others": YES,
			"org.freedesktop.udisks2.encrypted-unlock-other-seat": YES,
			"org.freedesktop.udisks2.eject-media-other-seat": YES,
			"org.freedesktop.udisks2.power-off-drive-other-seat": YES
		};
		if (subject.isInGroup("wheel")) {
			return permission[action.id];
		}
	});
	'';
	# Auto commit some repositories
	systemd.user.services.ggasy = {
		description = "Auto commit some git annex repository";
		wantedBy = [ "multi-user.target" ];
		serviceConfig = {
			Type = "oneshot";
			ExecStart = "/run/current-system/sw/bin/git-annex sync";
			WorkingDirectory="/home/vincent/desktop/org/";
			Environment = "PATH=/run/current-system/sw/bin";
		};
	};
	systemd.user.timers.ggasy = {
		description = "Auto commit hourly";
		wantedBy = [ "timers.target" ];
		timerConfig = {
			OnCalendar = "hourly";
			Persistent = "true";
		};
	};
	systemd.user.timers.ggasy.enable = true;
	# Auto refresh nix-channel each day
	systemd.user.services.channel-update = {
		description = "Update nix-channel daily";
		wantedBy = [ "multi-user.target" ];
		serviceConfig = {
			Type = "oneshot";
			ExecStart = "/run/current-system/sw/bin/nix-channel --update";
			Environment = "PATH=/run/current-system/sw/bin";
		};
	};
	systemd.user.timers.channel-update = {
		description = "Update nix-channel daily";
		wantedBy = [ "timers.target" ];
		timerConfig = {
			OnCalendar = "daily";
			Persistent = "true";
		};
	};
	systemd.user.timers.channel-update.enable = true;
}
