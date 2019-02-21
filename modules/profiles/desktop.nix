{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.desktop;
in
{
  options = {
    profiles.desktop = {
      enable = mkOption {
        default = false;
        description = "Enable desktop profile";
        type = types.bool;
      };
      avahi = mkOption {
        default = true;
        description = "Enable avahi  with the desktop profile";
        type = types.bool;
      };
      pulseaudio = mkOption {
        default = true;
        description = "Enable pulseaudio with the desktop profile";
        type = types.bool;
      };
      flatpak = mkOption {
        default = true;
        description = "Enable flatpak with the desktop profile";
        type = types.bool;
      };
      syncthing = mkOption {
        default = true;
        description = "Enable syncthing with the desktop profile";
        type = types.bool;
      };
      scanning = mkOption {
        default = true;
        description = "Enable scanning with the desktop profile";
        type = types.bool;
      };
      printing = mkOption {
        default = true;
        description = "Enable printing with the desktop profile";
        type = types.bool;
      };
      networkmanager = mkOption {
        default = true;
        description = "Enable networkmanager with the desktop profile";
        type = types.bool;
      };
      displayManager = mkOption {
        default = true;
        description = "Wether use a display manager or not";
        type = types.bool;
      };
      slimTheme = mkOption {
        default = {
          url = "https://github.com/vdemeester/slim-themes/raw/master/v-theme-0.1.tar.xz";
          sha256 = "1648krzmh6y2khbcf1zyik3znjpa8rckchbq49z1vqcg8zi587xi";
        };
        description = "Slim theme to use";
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      profiles.avahi.enable = cfg.avahi;
      profiles.printing.enable = cfg.printing;
      profiles.pulseaudio.enable = cfg.pulseaudio;
      profiles.scanning.enable = cfg.scanning;
      profiles.syncthing.enable = cfg.syncthing;
  
      boot = {
        tmpOnTmpfs = true;
        plymouth.enable = true;
      };
      
      networking.networkmanager = {
        enable = cfg.networkmanager;
        unmanaged =  [
          "interface-name:ve-*" "interface-name:veth*" "interface-name:wg0" "interface-name:docker0" "interface-name:virbr*"
        ];
        packages = with pkgs; [ networkmanager-openvpn ];  
      };
  
      programs.dconf.enable = true;
  
      services = {
        flatpak.enable = cfg.flatpak;
        dbus.packages = [ pkgs.gnome3.dconf ];
        xserver = {
          enable = true;
          enableTCP = false;
          windowManager.twm.enable = true;
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
              #MatchVendor     "ErgoDox_EZ"
              #MatchProduct    "ErgoDox_EZ"
              MatchIsKeyboard "on"
              MatchUSBID      "feed:1307"
              Driver          "evdev"
              Option          "XkbLayout"     "fr"
              Option          "XkbVariant"    "bepo"
            ''
          ];
          displayManager = {
            slim = {
              enable = cfg.displayManager;
              # Probably put this into users instead ?
              defaultUser = "vincent";
              theme = pkgs.fetchurl cfg.slimTheme;
            };
          };
        };
      };
      
      fonts = {
        enableFontDir = true;
        enableGhostscriptFonts = true;
        fonts = with pkgs; [
          corefonts
          dejavu_fonts
          emojione
          feh
          fira
          fira-code
          fira-code-symbols
          fira-mono
          font-droid
          hasklig
          inconsolata
          iosevka
          overpass
          symbola
          source-code-pro
          ubuntu_font_family
          unifont
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
  
      environment.systemPackages = with pkgs; [
        cryptsetup
        xlibs.xmodmap
        xorg.xbacklight
        xorg.xdpyinfo
        xorg.xhost
        xorg.xinit
        xss-lock
        xorg.xmessage
        unzip
        gnupg
        pinentry
        # user repositories
        nur.repos.mic92.inxi
      ];
    }
    (mkIf (!cfg.displayManager) {
      services.mingetty = {
        autologinUser = "vincent";
      };
    })
  ]);
}
