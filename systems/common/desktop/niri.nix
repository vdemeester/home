{
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ./tiling-common.nix
  ];

  environment.systemPackages = with pkgs; [
    xwayland-satellite
    wlogout
  ];

  xdg.portal = {
    config.niri = {
      default = [
        "gnome"
        "gtk"
      ];
      "org.freedesktop.impl.portal.Access" = "gtk";
      "org.freedesktop.impl.portal.Notification" = "gtk";
      "org.freedesktop.impl.portal.Secret" = "gnome-keyring";
      "org.freedesktop.impl.portal.FileChooser" = "gtk";
      "org.freedesktop.impl.portal.ScreenCast" = "gnome";
      "org.freedesktop.portal.ScreenCast" = "gnome";
    };
  };

  # Allow swaylock to unlock the computer for us
  security.pam.services.swaylock = {
    text = "auth include login";
  };

  # services.greetd.settings = {
  #   default_session = {
  #     # command = "${pkgs.greetd.greetd}/bin/agreety --cmd niri";
  #     command = "${
  #       lib.makeBinPath [ pkgs.greetd.tuigreet ]
  #     }/tuigreet -r --asterisks --time --cmd ${niriRun}";
  #     users = "greeter";
  #   };
  #   initial_session = {
  #     command = "${niriRun}";
  #     user = "vincent";
  #   };
  # };
}
