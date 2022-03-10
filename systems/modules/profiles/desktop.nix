{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.desktop;
in
{
  options = {
    profiles.desktop = {
      enable = mkEnableOption "Enable desktop profile";
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
    };
  };
  config = mkIf cfg.enable {
    profiles.avahi.enable = cfg.avahi;
    profiles.printing.enable = cfg.printing;
    profiles.pulseaudio.enable = cfg.pulseaudio;
    profiles.scanning.enable = cfg.scanning;
    profiles.syncthing.enable = cfg.syncthing;

    hardware.bluetooth.enable = true;

    networking.networkmanager = {
      enable = cfg.networkmanager;
      unmanaged = [
        "interface-name:br-*"
        "interface-name:ve-*"
        "interface-name:veth*"
        "interface-name:wg0"
        "interface-name:docker0"
        "interface-name:virbr*"
      ]; # FIXME: add unmanaged depending on profiles (wg0, docker0, …)
      packages = with pkgs; [ networkmanager-openvpn ];
    };

    services = {
      xserver = {
        enable = true;
        enableTCP = false;
        libinput.enable = true;
        synaptics.enable = false;
        layout = "fr";
        xkbVariant = "bepo";
        xkbOptions = "grp:menu_toggle,grp_led:caps,compose:caps";
      };
    };
    fonts = {
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [
        liberation_ttf
        corefonts
        dejavu_fonts
        emojione
        feh
        fira
        fira-code
        fira-code-symbols
        fira-mono
        hasklig
        inconsolata
        iosevka
        noto-fonts
        noto-fonts-cjk
        noto-fonts-emoji
        noto-fonts-extra
        overpass
        symbola
        source-code-pro
        twemoji-color-font
        ubuntu_font_family
        unifont
      ];
    };

    environment.systemPackages = with pkgs; [
      cryptsetup
      xorg.xmodmap
      # xorg.xbacklight
      xorg.xdpyinfo
      xorg.xhost
      xorg.xinit
      xss-lock
      xorg.xmessage
      unzip
      gnupg
      pinentry
      inxi
    ];
  };
}
