{ config, pkgs, ... }:

{
  imports =
    [
      <nixpkgs/nixos/modules/services/hardware/sane_extra_backends/brscan4.nix>
    ];
	environment = {
	systemPackages = with pkgs; [
		dmenu2
		dunst
		emacs
		firefox
		# google-chrome
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
		xlibs.xmodmap
		xorg.xbacklight
		xss-lock
		];
	};

	hardware.opengl.extraPackages = [ pkgs.vaapiIntel ];
	hardware.sane = {
		enable = true;
		brscan4.enable = true;
		brscan4.netDevices = {
      			docker = { model = "MFC-9330CDW"; ip = "192.168.1.57"; };
    		};
	};
	services = {
		printing = {
			enable = true;
			drivers = [ pkgs.gutenprint ];
		};
		xserver = {
			enable = true;
			enableTCP = false;
			libinput.enable = true;
			windowManager = {
				i3 = {
					enable = true;
				};
				default = "i3";
			};
			displayManager = {
				slim = {
					enable = true;
					defaultUser = "vincent";
				};
				sessionCommands = ''
${pkgs.xss-lock}/bin/xss-lock i3lock -- -i $HOME/.background-image &
${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.Xmodmap &
${pkgs.networkmanagerapplet}/bin/nm-applet &
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
			symbola
			fira
			fira-code
			fira-mono
			font-droid
		];
	};
}
