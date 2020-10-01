{ lib, ... }:
let
  mkTuple = lib.hm.gvariant.mkTuple;
in
{
  dconf.settings = {
    # Interface
    "org/gnome/desktop/interface" = {
      clock-show-date = true;
      clock-show-seconds = false;
      clock-show-weekday = true;
      # enable-animations=false;
      monospace-font-name = "Ubuntu Mono 13";
      show-battery-percentage = true;
      toolkit-accessibility = false;
    };
    # Inputs
    "org/gnome/desktop/input-sources" = {
      "current" = "uint32 0";
      "sources" = [ (mkTuple [ "xkb" "fr+bepo" ]) (mkTuple [ "xkb" "us" ]) ];
      "xkb-options" = [ "lv3:ralt_switch" "caps:ctrl_modifier" ];
    };
    # Window manager
    "org/gnome/desktop/wm/keybindings" = {
      activate-window-menu = [ "<Shift><Alt>nobreakspace" ];
      close = [ "<Shift><Super>b" ];
      panel-run-dialog = [ "<Super>j" ];
      switch-applications = [ "<Alt>Tab" ];
      switch-applications-backward = [ "<Shift><Alt>Tab" ];
      switch-windows = [ "<Super>Tab" ];
      switch-windows-backward = [ "<Shift><Super>Tab" ];
      toggle-fullscreen = [ "<Alt>F11" ];
    };
    "org/gnome/desktop/wm/preferences" = {
      action-middle-click-titlebar = "none";
      auto-raise = true;
      button-layout = "appmenu:close";
      focus-new-windows = "strict";
      resize-with-right-button = true;
    };
    # Background and screensaver
    "org/gnome/desktop/background" = {
      "picture-uri" = "file:///home/vincent/desktop/pictures/wallpapers/dynamics/firewatch/firewatch.xml";
    };
    "org/gnome/desktop/screensaver" = {
      "picture-uri" = "file:///home/vincent/desktop/pictures/wallpapers/dynamics/firewatch/firewatch.xml";
    };
    # settings
    "org/gnome/settings-daemon/plugins/color" = {
      night-light-enabled = true;
      night-light-last-coordinates = (mkTuple [ 48.844400719942406 2.3488000000000002 ]);
    };
    # Tilix
    "com/gexperts/Tilix" = {
      "control-scoll-zoom" = true;
      "notify-on-process-complete" = true;
      "prompt-on-close" = true;
      "theme-variant" = "dark";
    };
    "com/gexperts/Tilix/keybindings" = {
      "session-add-down" = "<Primary><Alt>d";
    };
    "com/gexperts/Tilix/profiles" = {
      list = [ "2b7c4080-0ddd-46c5-8f23-563fd3ba789d" "3c416d51-7dbc-4c73-8448-de56ed4ac3c2" ];
    };
    "com/gexperts/Tilix/profiles/2b7c4080-0ddd-46c5-8f23-563fd3ba789d" = {
      background-color = "#272C34";
      badge-color-set = false;
      bold-color-set = false;
      cursor-colors-set = false;
      font = "Fira Code weight=450 13";
      foreground-color = "#BBC2CF";
      highlight-colors-set = false;
      palette = [ "#272C34" "#FF6C6B" "#98BE65" "#ECBE7A" "#50AFEF" "#C678DD" "#46D9FF" "#BBC2CF" "#4A4A4F4F5555" "#FF6C6B" "#98BE65" "#ECBE7A" "#50AFEF" "#C678DD" "#46D9FF" "#BBC2CF" ];
      shortcut = "<Primary><Shift><Alt>d";
      use-system-font = true;
      use-theme-colors = false;
      visible-name = "Default";
    };
    "com/gexperts/Tilix/profiles/3c416d51-7dbc-4c73-8448-de56ed4ac3c2" = {
      background-color = "#BBBBC2C2CFCF";
      badge-color = "#AC7EA8";
      badge-color-set = false;
      bold-color-set = false;
      cursor-colors-set = false;
      font = "Fira Code weight=450 12";
      foreground-color = "#26262C2C3434";
      highlight-colors-set = false;
      palette = [ "#272C34" "#FF6C6B" "#98BE65" "#ECBE7A" "#50AFEF" "#C678DD" "#46D9FF" "#BBC2CF" "#272C34" "#FF6C6B" "#98BE65" "#ECBE7A" "#50AFEF" "#C678DD" "#46D9FF" "#BBC2CF" ];
      shortcut = "<Primary><Shift><Alt>l";
      use-system-font = true;
      use-theme-colors = false;
      visible-name = "Light";
    };
    # Shell
    "org/gnome/shell" = {
      enabled-extensions = [ "drive-menu@gnome-shell-extensions.gcampax.github.com" "sound-output-device-chooser@kgshank.net" "user-theme@gnome-shell-extensions.gcampax.github.com" "launch-new-instance@gnome-shell-extensions.gcampax.github.com" "bluetooth-quick-connect@bjarosze.gmail.com" ];
    };
    # Extensions
    "org/gnome/shell/extensions/hidetopbar" = {
      enable-active-window = false;
      enable-intellihide = true;
      mouse-sensitive = true;
      mouse-sensitive-fullscreen-window = false;
    };
  };
}
