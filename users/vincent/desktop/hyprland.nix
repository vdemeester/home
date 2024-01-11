{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    qogir-icon-theme
    cliphist
  ];
  xdg.configFile."hypr/hyprpaper.conf".text = ''
    preload = /home/vincent/desktop/pictures/lockscreen
    wallpaper = , /home/vincent/desktop/pictures/lockscreen
    ipc = off
  '';
  wayland.windowManager.hyprland = {
    enable = true;
    systemd.enable = true;
    xwayland.enable = true;

    settings = {
      env = [
        "QT_WAYLAND_DISABLE_WINDOWDECORATION,1"
        "SDL_VIDEODRIVER,wayland"
        "QT_QPA_PLATFORM,wayland"
        "_JAVA_AWT_WM_NONREPARTENTING,1"
        "MOZ_ENABLE_WAYLAND,1"
      ];

      exec-once = [
        "${pkgs.waybar}/bin/waybar -c ~/.config/waybar/config "
        "hyprctl setcursor Qogir 24"
        "wl-paste -p --watch cliphist store"
        "${pkgs.hyprpaper}/bin/hyprpaper"
        "${pkgs.networkmanagerapplet}/bin/nm-applet --indicator"
        # kanshi ? (systemctl --user restart kanshi)
      ];

      animations = {
        enabled = "yes";
        bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";
        animation = [
          "windows, 1, 5, myBezier"
          "windowsOut, 1, 7, default, popin 80%"
          "border, 1, 10, default"
          "fade, 1, 7, default"
          "workspaces, 1, 6, default"
        ];
      };

      monitor = [
        # "eDP-1, 1920x1080, 0x0, 1"
        # "HDMI-A-1, 2560x1440, 1920x0, 1"
        # Old: Output eDP-1 'AU Optronics 0xD291 Unknown'
        # Output eDP-1 'Unknown 0xD291 Unknown'
        # Output DP-5 'LG Electronics LG ULTRAWIDE 0x0005D10C' (focused)
        "eDP-1,preferred,0x0,1" # or 1460,1440
        "DP-5,3440x1440,-1520x1440,1"
        ",preferred,auto,1"
      ];

      input = {
        kb_layout = "fr";
        kb_variant = "bepo";
        kb_options = "grp:menu_toggle,grp_led:caps,compose:caps";

        follow_mouse = 1;
      };

      "$mod" = "SUPER";
      bind = [
        "$mod, Return, exec, kitty"
        "$mod CTRL, Return, exec, emacs"
        "$mod SHIFT, Return, exec, emacsclient -c"

        "$mod, BACKSPACE, focuscurrentorlast"

        "$mod SHIFT, code:24, killactive,"
        "$mod, Q, exit,"
        "$mod SHIFT, Space, togglefloating,"
        "$mod, R, exec, wofi --show drun"
        # "$mod, P, pseudo, # dwindle"
        "$mod, P, togglesplit, # dwindle"

        "$mod, code:41, fullscreen"

        "$mod CTRL, code:33, exec, ${pkgs.wofi-emoji}/bin/wofi-emoji -G"
        "$mod, code:33, exec, ${pkgs.wofi}/bin/wofi -G --show drun -modi 'drun,run,window,ssh'"

        # Move focus with mainMod + arrow keys
        "$mod, left, movefocus, l"
        "$mod, right, movefocus, r"
        "$mod, up, movefocus, u"
        "$mod, down, movefocus, d"

        "$mod SHIFT, left, moveactive, l"
        "$mod SHIFT, right, moveactive, r"
        "$mod SHIFT, up, moveactive, u"
        "$mod SHIFT, down, moveactive, d"

        "$mod CTRL, left, workspace, e-1" # FIXME: adapt ?
        "$mod CTRL, right, workspace, e+1" # FIXME: adapt ?
        "$mod CTRL, down, workspace, e-1"
        "$mod CTRL, up, workspace, e+1"
        "$mod SHIFT CTRL, left, movetoworkspace, e-1" # FIXME: adapt ?
        "$mod SHIFT CTRL, right, movetoworkspace, e+1" # FIXME: adapt ?
        "$mod SHIFT CTRL, down, movetoworkspace, e-1"
        "$mod SHIFT CTRL, up, movetoworkspace, e+1"

        # Scroll through existing workspaces with mainMod + scroll
        "$mod, mouse_down, workspace, e+1"
        "$mod, mouse_up, workspace, e-1"

        # Switch workspaces with mainMod + [0-9]
        "$mod, code:10, workspace, 1"
        "$mod, code:11, workspace, 2"
        "$mod, code:12, workspace, 3"
        "$mod, code:13, workspace, 4"
        "$mod, code:14, workspace, 5"
        "$mod, code:15, workspace, 6"
        "$mod, code:16, workspace, 7"
        "$mod, code:17, workspace, 8"
        "$mod, code:18, workspace, 9"
        "$mod, code:19, workspace, 10"

        # Move active window to a workspace with mainMod + SHIFT + [0-9]
        "$mod SHIFT, code:10, movetoworkspace, 1"
        "$mod SHIFT, code:11, movetoworkspace, 2"
        "$mod SHIFT, code:12, movetoworkspace, 3"
        "$mod SHIFT, code:13, movetoworkspace, 4"
        "$mod SHIFT, code:14, movetoworkspace, 5"
        "$mod SHIFT, code:15, movetoworkspace, 6"
        "$mod SHIFT, code:16, movetoworkspace, 7"
        "$mod SHIFT, code:17, movetoworkspace, 8"
        "$mod SHIFT, code:18, movetoworkspace, 9"
        "$mod SHIFT, code:19, movetoworkspace, 10"

        # Example special workspace (scratchpad)
        "$mod, code:49, togglespecialworkspace, magic"
        "$mod SHIFT, code:49, movetoworkspace, special:magic"

        # Media CTRLs
        ", XF86AudioRaiseVolume, exec, ${pkgs.pamixer}/bin/pamixer -ui 5"
        ", XF86AudioLowerVolume, exec, ${pkgs.pamixer}/bin/pamixer -ud 5"
        ", XF86AudioMicMute, exec, ${pkgs.pamixer}/bin/pamixer --default-source -m"
        ", XF86AudioMute, exec, ${pkgs.pamixer}/bin/pamixer -m"
        ", XF86AudioPlay, exec, playerctl play-pause"
        ", XF86AudioPause, exec, playerctl play-pause"
        ", XF86Messenger, exec, playerctl play-pause"
        ", XF86AudioNext, exec, playerctl next"
        ", XF86Go, exec, playerctl next"
        ", XF86AudioPrev, exec, playerctl previous"
        ", Cancel, exec, playerctl previous"

        "$mod, F10, exec, ${pkgs.my.scripts}/bin/shot %d"
        "$mod SHIFT, F10, exec, ${pkgs.my.scripts}/bin/shotf %d"

        "$mod, F9, exec, ${pkgs.mako}/bin/makoctl mode -s do-not-disturb"
        "$mod SHIFT, F9, exec, ${pkgs.mako}/bin/makoctl mode -s default"
      ];
      bindm = [
        "$mod, mouse:273, resizewindow"
        "$mod, mouse:272, movewindow"
      ];
      misc = {
        force_default_wallpaper = 0; # -1 for no wallpaper, 0 for default wallpaper, 1 for custom wallpaper
      };
    };
  };
}
