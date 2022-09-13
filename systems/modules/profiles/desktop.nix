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
      audio = mkOption {
        default = true;
        description = "Enable audio with the desktop profile";
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
    profiles.printing.enable = cfg.printing;
    profiles.scanning.enable = cfg.scanning;

    modules = {
      hardware = {
        bluetooth.enable = true;
        audio.enable = cfg.audio;
      };
      services = {
        avahi.enable = cfg.avahi;
        syncthing.enable = cfg.syncthing;
      };
    };

    networking.networkmanager = {
      enable = cfg.networkmanager;
      unmanaged = [
        "interface-name:br-*"
        "interface-name:ve-*"
        "interface-name:veth*"
      ]
      # Do not manager wireguard
      ++ lib.optionals config.networking.wireguard.enable [ "interface-name:wg0" ]
      # Do not manage docker interfaces
      ++ lib.optionals config.virtualisation.docker.enable [ "interface-name:docker0" ]
      # Do not manager libvirt interfaces
      ++ lib.optionals config.virtualisation.libvirtd.enable [ "interface-name:virbr*" ];
      packages = with pkgs; [ networkmanager-openvpn ];
      dispatcherScripts = [{
        # https://askubuntu.com/questions/1271491/disable-wifi-if-lan-is-connected
        source = pkgs.writeText "wifi-wired-exclusive" ''
          #!${pkgs.bash}/bin/bash
          export LC_ALL=C

          enable_disable_wifi ()
          {
              result=$(${pkgs.networkmanager}/bin/nmcli dev | ${pkgs.gnugrep}/bin/grep "ethernet" | ${pkgs.gnugrep}/bin/grep -w "connected")
              if [ -n "$result" ]; then
                  ${pkgs.networkmanager}/bin/nmcli radio wifi off
              else
                  ${pkgs.networkmanager}/bin/nmcli radio wifi on
              fi
          }

          if [ "$2" = "up" ]; then
              enable_disable_wifi
          fi

          if [ "$2" = "down" ]; then
              enable_disable_wifi
          fi
        '';
        type = "basic";
      }];
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
