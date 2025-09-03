{ pkgs, ... }:
{
  programs = {
    dconf.enable = true;
  };

  environment = {
    variables.NIXOS_OZONE_WL = "1";

    systemPackages = with pkgs; [
      qogir-icon-theme
      libheif
      libheif.out
      polkit_gnome
    ];
  };

  services = {
    dbus = {
      enable = true;
      implementation = "broker"; # trying this one out
      packages = [
        pkgs.dconf
        pkgs.gcr
      ];
    };
    greetd = {
      enable = true;
      restart = false;
    };
    gvfs.enable = true; # trying this one out

    libinput = {
      touchpad = {
        disableWhileTyping = true;
        # additionalOptions = ''
        #   							Option "Ignore" "on"
        #   						'';
      };
    };
  };

  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = true;
    config = {
      common = {
        default = [
          "gnome"
          "gtk"
        ];
        "org.freedesktop.impl.portal.ScreenCast" = "gnome";
        "org.freedesktop.impl.portal.Screenshot" = "gnome";
        "org.freedesktop.impl.portal.RemoteDesktop" = "gnome";
      };
    };
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
      pkgs.xdg-desktop-portal-gnome
    ];
  };
}
