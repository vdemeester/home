{ lib, ... }:
let
  mkTuple = lib.hm.gvariant.mkTuple;
in
{
  dconf.settings = {
    "org/gnome/desktop/input-sources" = {
      "current" = "uint32 0";
      "sources" = [ (mkTuple [ "xkb" "fr+bepo" ]) (mkTuple [ "xkb" "us" ]) ];
      "xkb-options" = [ "lv3:ralt_switch" "caps:ctrl_modifier" ];
    };

    "org/gnome/desktop/background" = {
      "picture-uri" = "file:///home/vincent/desktop/pictures/wallpapers/dynamics/firewatch/firewatch.xml";
    };
    "org/gnome/desktop/screensaver" = {
      "picture-uri" = "file:///home/vincent/desktop/pictures/wallpapers/dynamics/firewatch/firewatch.xml";
    };
  };
}
