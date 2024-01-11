{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    qogir-icon-theme
  ];
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
        "${pkgs.waybar}"
        "hyprctl setcursor Qogir 24"
      ];

      monitor = [
        # Old: Output eDP-1 'AU Optronics 0xD291 Unknown'
        # Output eDP-1 'Unknown 0xD291 Unknown'
        # Output DP-5 'LG Electronics LG ULTRAWIDE 0x0005D10C' (focused)
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
        "$mod, F, exec, firefox"
        # "$mod, Shift, L, exec, swaylock -fF -c a6e3a1"
        "$mod, C, killactive,"
        "$mod, Q, exit,"
        "$mod, E, exec, dolphin"
        "$mod, V, togglefloating,"
        "$mod, R, exec, wofi --show drun"
        "$mod, P, pseudo, # dwindle"
        "$mod, J, togglesplit, # dwindle"

        # Move focus with mainMod + arrow keys
        "$mod, left, movefocus, l"
        "$mod, right, movefocus, r"
        "$mod, up, movefocus, u"
        "$mod, down, movefocus, d"

        # Switch workspaces with mainMod + [0-9]
        "$mod, 1, workspace, 1"
        "$mod, 2, workspace, 2"
        "$mod, 3, workspace, 3"
        "$mod, 4, workspace, 4"
        "$mod, 5, workspace, 5"
        "$mod, 6, workspace, 6"
        "$mod, 7, workspace, 7"
        "$mod, 8, workspace, 8"
        "$mod, 9, workspace, 9"
        "$mod, 0, workspace, 10"

        # Move active window to a workspace with mainMod + SHIFT + [0-9]
        "$mod SHIFT, 1, movetoworkspace, 1"
        "$mod SHIFT, 2, movetoworkspace, 2"
        "$mod SHIFT, 3, movetoworkspace, 3"
        "$mod SHIFT, 4, movetoworkspace, 4"
        "$mod SHIFT, 5, movetoworkspace, 5"
        "$mod SHIFT, 6, movetoworkspace, 6"
        "$mod SHIFT, 7, movetoworkspace, 7"
        "$mod SHIFT, 8, movetoworkspace, 8"
        "$mod SHIFT, 9, movetoworkspace, 9"
        "$mod SHIFT, 0, movetoworkspace, 10"

        # Example special workspace (scratchpad)
        "$mod, $, togglespecialworkspace, magic"
        "$mod SHIFT, $, movetoworkspace, special:magic"

        # Scroll through existing workspaces with mainMod + scroll
        "$mod, mouse_down, workspace, e+1"
        "$mod, mouse_up, workspace, e-1"


        # Media controls
        ", XF86AudioRaiseVolume, exec, pamixer -i 5"
        ", XF86AudioLowerVolume, exec, pamixer -d 5"
        ", XF86AudioMicMute, exec, pamixer --default-source -m"
        ", XF86AudioMute, exec, pamixer -m"
        ", XF86AudioPlay, exec, playerctl play-pause"
        ", XF86AudioPause, exec, playerctl play-pause"
        ", XF86AudioNext, exec, playerctl next"
        ", XF86AudioPrev, exec, playerctl previous"
      ];
      # The default keybindings are:
      #   Mod4 + Enter: Launch terminal
      #   Mod4 + Shift + Enter: Launch dmenu
      #   Mod4 + Shift + q: Quit
      #   Mod4 + Shift + r: Restart
      #   Mod4 + Shift + c: Close window
      #   Mod4 + Shift + t: Toggle tiling
      #   Mod4 + Shift + f: Toggle fullscreen
      #   Mod4 + Shift + m: Toggle monocle
      #   Mod4 + Shift + s: Toggle sticky
      #   Mod4 + Shift + n: Toggle floating
      #   Mod4 + Shift + h: Decrease master size
      #   Mod4 + Shift + l: Increase master size
      #   Mod4 + Shift + j: Focus next window
      #   Mod4 + Shift + k: Focus previous window
      #   Mod4 + Shift + space: Focus master window
      #   Mod4 + Shift + 1-9: Switch to workspace 1-9
      #   Mod4 + Shift + 0: Switch to last workspace
      #   Mod4 + Shift + Tab: Switch to last workspace
      #   Mod4 + Shift + Shift + 1-9: Move window to workspace 1-9
      #   Mod4 + Shift + Shift + 0: Move window to last workspace
      #   Mod4 + Shift + Shift + Tab: Move window to last workspace
      #   Mod4 + Shift + Shift + h: Move window to left
      #   Mod4 + Shift + Shift + l: Move window to right
      #   Mod4 + Shift + Shift + j: Move window to down
      #   Mod4 + Shift + Shift + k: Move window to up
      #   Mod4 + Shift + Shift + space: Toggle floating
      #   Mod4 + Shift + Shift + f: Toggle fullscreen
      #   Mod4 + Shift + Shift + m: Toggle monocle
      #   Mod4 + Shift + Shift + s: Toggle sticky
      #   Mod4 + Shift + Shift + c: Close window
      #   Mod4 + Shift + Shift + t: Toggle tiling
      #   Mod4 + Shift + Shift + r: Restart
      #   Mod
      misc = {
        force_default_wallpaper = -1; # -1 for no wallpaper, 0 for default wallpaper, 1 for custom wallpaper
      };
    };
  };
}
