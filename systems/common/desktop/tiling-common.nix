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
      packages = [ pkgs.dconf pkgs.gcr ];
    };
    greetd = {
      enable = true;
      restart = false;
    };
    gvfs.enable = true; # trying this one out

    libinput = {
      touchpad = {
        disableWhileTyping = true;
        additionalOptions = ''
          							Option "Ignore" "on"
          						'';
      };
    };
  };

  xdg = {
    portal = {
      enable = true;
      wlr.enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
    };
  };
}
