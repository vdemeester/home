{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkEnableOption mkDefault;
  cfg = config.modules.desktop;
in
{
  options = {
    modules.desktop = {
      enable = mkEnableOption "desktop configuration";
    };
  };
  config = mkIf cfg.enable {
    boot = {
      # /tmp to be tmpfs
      tmpOnTmpfs = true;
      # Enable Plymouth on desktops
      plymouth.enable = true;
    };

    # FIXME Fix tmpOnTmpfs
    systemd.additionalUpstreamSystemUnits = [ "tmp.mount" ];

    # Extra packages to add to the system
    environment.systemPackages = with pkgs; [
      xorg.xmessage
    ];

    # Configure some fonts
    fonts = {
      # enableFontDir = true;
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
        go-font
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
    };

    nix = {
      # Enable SSH-serving nix packages
      sshServe.enable = mkDefault true;
    };

    services = {
      # Enable avahi with a lot of options
      avahi = {
        enable = true;
        ipv4 = true;
        ipv6 = true;
        nssmdns = true;
        publish = {
          enable = true;
          userServices = true;
        };
      };

      # Make `/run/user/X` larger.
      logind.extraConfig = ''
        RuntimeDirectorySize=20%
      '';

      # Enable printing by default too
      printing = {
        enable = true;
        drivers = [ pkgs.gutenprint ];
      };

      # Enable xserver on desktop
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
  };
}
