{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkEnableOption mkDefault mkOption types;
  cfg = config.modules.desktop;
in
{
  options = {
    modules.desktop = {
      enable = mkEnableOption "desktop configuration";
      plymouth = {
        theme = mkOption {
          default = "deus_ex";
          description = "Plymouth theme to use for boot (hexagon, green_loader, deus_ex, cuts, sphere, spinner_alt)";
          type = types.str;
        };
        themePackage = mkOption {
          default = pkgs.my.adi1090x-plymouth;
          description = "Plymouth theme package to use";
          type = types.package;
        };
      };
    };
  };
  config = mkIf cfg.enable {
    modules.services.avahi.enable = true;
    # Enable netbootxyz if systemd-boot is enabled
    boot = {
      loader.systemd-boot.netbootxyz.enable = config.core.boot.systemd-boot;
      # /tmp to be tmpfs
      tmpOnTmpfs = true;
      # Enable Plymouth on desktops
      plymouth = {
        enable = true;
        themePackages = [ cfg.plymouth.themePackage ];
        theme = cfg.plymouth.theme;
      };
    };

    # Configure some fonts
    fonts = {
      # enableFontDir = true;
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [
        liberation_ttf
        cascadia-code
        corefonts
        dejavu_fonts
        emojione
        feh
        fira
        fira-code
        fira-code-symbols
        fira-code-nerdfont
        fira-mono
        font-awesome
        go-font
        hasklig
        inconsolata
        inconsolata-nerdfont
        iosevka
        iosevka-comfy.comfy
        jetbrains-mono
        nerdfonts
        noto-fonts
        noto-fonts-cjk
        noto-fonts-emoji
        noto-fonts-extra
        overpass
        symbola
        sudo-font
        source-code-pro
        twemoji-color-font
        ubuntu_font_family
        unifont
        victor-mono
      ];
    };

    # Enable NetkworManager by default
    networking.networkmanager = {
      enable = mkDefault true;
      unmanaged = [
        "interface-name:br-*"
        "interface-name:ve-*" # FIXME are those docker's or libvirt's
        "interface-name:veth-*" # FIXME are those docker's or libvirt's
      ]
      # Do not manager wireguard
      ++ lib.optionals config.networking.wireguard.enable [ "interface-name:wg0" ]
      # Do not manage docker interfaces
      ++ lib.optionals config.virtualisation.docker.enable [ "interface-name:docker0" ]
      # Do not manager libvirt interfaces
      ++ lib.optionals config.virtualisation.libvirtd.enable [ "interface-name:virbr*" ];
      packages = with pkgs; [ networkmanager-openvpn ];
      # dispatcherScripts = [{
      #   # https://askubuntu.com/questions/1271491/disable-wifi-if-lan-is-connected
      #   source = pkgs.writeText "wifi-wired-exclusive" ''
      #     #!${pkgs.bash}/bin/bash
      #     export LC_ALL=C
      # 
      #     enable_disable_wifi ()
      #     {
      #         result=$(${pkgs.networkmanager}/bin/nmcli dev | ${pkgs.gnugrep}/bin/grep "ethernet" | ${pkgs.gnugrep}/bin/grep -w "connected")
      #         if [ -n "$result" ]; then
      #             ${pkgs.networkmanager}/bin/nmcli radio wifi off
      #         else
      #             ${pkgs.networkmanager}/bin/nmcli radio wifi on
      #         fi
      #     }
      # 
      #     if [ "$2" = "up" ]; then
      #         enable_disable_wifi
      #     fi
      # 
      #     if [ "$2" = "down" ]; then
      #         enable_disable_wifi
      #     fi
      #   '';
      #   type = "basic";
      # }];
    };

    nix = {
      # Enable SSH-serving nix packages
      sshServe.enable = mkDefault true;
    };

    services = {
      udisks2.enable = true;

      # Make `/run/user/X` larger.
      logind.extraConfig = ''
        RuntimeDirectorySize=20%
      '';

      # Enable printing by default too
      printing = {
        enable = true;
        drivers = [ pkgs.gutenprint ];
      };
    };
  };
}
