{ pkgs, config, lib, ... }:

{
  xsession.windowManager.i3 = {
    enable = true;
    config = {
      fonts = ["Fira Code 10"];
      focus = {
        followMouse = false;
      };
      window = {
        titlebar = false;
        border = 1;
        hideEdgeBorders = "both";
      };
      keybindings = {
        "Mod4+Return" = "exec alacritty";
        "Mod4+Shift+F11" = "exec 'autorandr -c'";
      };
      keycodebindings = {
        "Mod4+Shift+24" = "kill";
        "Mod4+33" = "exec \"${pkgs.rofi}/bin/rofi -show run -modi 'run,window' -kb-row-select 'Tab' -kb-row-tab '' -location 2 -hide-scrollbar -separator-style solid -font 'Ubuntu Mono 14'";
        # focus window
        "Mod4+44" = "focus left";
        "Mod4+45" = "focus down";
        "Mod4+46" = "focus up";
        "Mod4+47" = "focus right";
        "Mod4+38" = "focus parent";
        # move focused window
        "Mod4+Shift+44" = "move left";
        "Mod4+Shift+45" = "move down";
        "Mod4+Shift+46" = "move up";
        "Mod4+Shift+47" = "move right";
        # Split
        "Mod4+43" = "split h";
        "Mod4+55" = "split v";
        # Fullscreen
        "Mod4+41" = "fullscreen toggle";
        # Change container layout
        "Mod4+39" = "layout stacking";
        "Mod4+25" = "layout tabbed";
        "Mod4+26" = "layout toggle split";
        # Manage floating
        "Mod4+Shift+61" = "floating toggle";
        "Mod4+61" = "focus mode_toggle";
        # manage workspace
        "Mod4+113" = "workspace prev_on_output";
        "Mod4+114" = "workspace next_on_output";
        # manage output
        "Mod4+Shift+113" = "focus output left";
        "Mod4+Shift+116" = "focus output down";
        "Mod4+Shift+111" = "focus output up";
        "Mod4+Shift+114" = "focus output right";
        # Custom keybinding
        "Mod4+Shift+32" = "exec loginctl lock-session";
        "Mod4+Shift+39" = "exec sleep 1 && xset dpms force standby";
        "Mod4+24" = "border toggle";
      };
      modes = {};
      bars = [{
        mode = "hide";
        position = "bottom";
        statusCommand = "${pkgs.i3status}/bin/i3status";
        # fonts = ["Fira Code 10"];
        colors = {
          background = "#073642";
          statusline = "#eee8d5";
          focusedWorkspace = {
            border = "#cb4b16";
            background = "#cb4b16";
            text = "#eee8d5";
          };
          activeWorkspace = {
            border = "#cb4b16";
            background = "#cb4b16";
            text = "#eee8d5";
          };
          inactiveWorkspace = {
            border = "#b58900";
            background = "#b58900";
            text = "#eee8d5";
          };
        };
      }];
    };
    extraConfig = ''
      set $mod Mod4

      # Use Mouse+$mod to drag floating windows to their wanted position
      floating_modifier $mod

      set $WS0 0 🐽
      set $WS1 1 🌎
      set $WS2 2 🐧
      set $WS3 3 🐹
      set $WS4 4 🐸
      set $WS5 5 👷
      set $WS6 6 🔰
      set $WS7 7 ꙮ
      set $WS8 8 🎧
      set $WS9 9 🖃
      # 🗍 🖳 🖧 🖃 🔰 🔮 📰 📐 📁 📂 💻 💡 💢 👷 👊
      # 🐳 🐸 🐹 🐺 🐽 🐮 🐾 🐿 🐧 🐥 🐣 🐠 🐘 🐙 🐟 🏭
      # 🏈 🎧 🍰 🍪 🍙 🌵 🌟 ⛺ ⚗ ♺

      # switch to workspace
      bindcode $mod+10 workspace $WS1
      bindcode $mod+11 workspace $WS2
      bindcode $mod+12 workspace $WS3
      bindcode $mod+13 workspace $WS4
      bindcode $mod+14 workspace $WS5
      bindcode $mod+15 workspace $WS6
      bindcode $mod+16 workspace $WS7
      bindcode $mod+17 workspace $WS8
      bindcode $mod+18 workspace $WS9
      bindcode $mod+19 workspace $WS0

      # move focused container to workspace
      bindcode $mod+Shift+10 move container to workspace $WS1
      bindcode $mod+Shift+11 move container to workspace $WS2
      bindcode $mod+Shift+12 move container to workspace $WS3
      bindcode $mod+Shift+13 move container to workspace $WS4
      bindcode $mod+Shift+14 move container to workspace $WS5
      bindcode $mod+Shift+15 move container to workspace $WS6
      bindcode $mod+Shift+16 move container to workspace $WS7
      bindcode $mod+Shift+17 move container to workspace $WS8
      bindcode $mod+Shift+18 move container to workspace $WS9
      bindcode $mod+Shift+19 move container to workspace $WS0

      assign [class="Firefox" window_role="browser"] → $WS1
      assign [class="Google-chrome" window_role="browser"] → $WS1

      ## quick terminal (tmux)
      exec --no-startup-id alacritty --title metask --class metask --command tmux
      for_window [instance="metask"] floating enable;
      for_window [instance="metask"] move scratchpad; [instance="metask"] scratchpad show; move position 0px 0px; move scratchpad
      bindcode $mod+49 [instance="metask"] scratchpad show

      bindsym XF86MonBrightnessUp exec "xbacklight -inc 10"
      bindsym XF86MonBrightnessDown exec "xbacklight -dec 10"
      bindsym shift+XF86MonBrightnessUp exec "xbacklight -inc 1"
      bindsym shift+XF86MonBrightnessDown exec "xbacklight -dec 1"
      bindsym XF86AudioLowerVolume exec "pactl set-sink-mute @DEFAULT_SINK@ false ; pactl set-sink-volume @DEFAULT_SINK@ -5%"
      bindsym XF86AudioRaiseVolume exec "pactl set-sink-mute @DEFAULT_SINK@ false ; pactl set-sink-volume @DEFAULT_SINK@ +5%"
      bindsym XF86AudioMute exec "pactl set-sink-mute @DEFAULT_SINK@ toggle"
      bindsym XF86AudioMicMute exec "pactl set-source-mute @DEFAULT_SOURCE@ toggle"
      bindsym XF86AudioPlay exec "playerctl play-pause"
      bindsym XF86AudioNext exec "playerctl next"
      bindsym XF86AudioPrev exec "playerctl previous"

      # reload the configuration file
      bindsym $mod+Shift+x reload
      # restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
      bindsym $mod+Shift+o restart
      # exit i3 (logs you out of your X session)
      bindsym $mod+Shift+p exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3?' -b 'Yes, exit i3' 'i3-msg exit'"
      # poweroff
      bindsym $mod+Shift+F12 exec "i3-nagbar -t warning -m 'You pressed the poweroff shortcut. Do you really want to poweroff?' -b 'Yes, poweroff' 'systemctl poweroff'"
      # reboot
      bindsym $mod+Control+F12 exec "i3-nagbar -t warning -m 'You pressed the reboot shortcut. Do you really want to reboot?' -b 'Yes, reboot' 'systemctl reboot'"
      # move workspace to output
      set $workspace_move Move workspace to output : [l]eft [r]ight [d]own [u]p

      mode "$workspace_move" {
      bindsym left move workspace to output left
      bindsym l move workspace to output left

      bindsym right move workspace to output right
      bindsym r move workspace to output right

      bindsym down move workspace to output down
      bindsym d move workspace to output down

      bindsym up move workspace to output up
      bindsym u move workspace to output up

      bindsym Escape mode "default"
      bindsym Return mode "default"
      }

      bindsym $mod+m mode "$workspace_move"

      # resize window (you can also use the mouse for that)
      mode "resize" {
      # These bindings trigger as soon as you enter the resize mode

      # Pressing left will shrink the window’s width.
      # Pressing right will grow the window’s width.
      # Pressing up will shrink the window’s height.
      # Pressing down will grow the window’s height.
      bindsym t resize shrink width 10 px or 10 ppt
      bindsym s resize grow height 10 px or 10 ppt
      bindsym r resize shrink height 10 px or 10 ppt
      bindsym n resize grow width 10 px or 10 ppt

      # same bindings, but for the arrow keys
      bindsym Left resize shrink width 10 px or 10 ppt
      bindsym Down resize grow height 10 px or 10 ppt
      bindsym Up resize shrink height 10 px or 10 ppt
      bindsym Right resize grow width 10 px or 10 ppt

      # back to normal: Enter or Escape
      bindsym Return mode "default"
      bindsym Escape mode "default"
      }

      bindsym $mod+o mode "resize"
    '';
  };
  xdg.configFile."i3/i3status.conf".text = ''
    general {
    colors = true
    color_bad = "#dc322f"
    color_degraded = "#cb4b16"
    color_separator = "#000000"
    output_format = "i3bar"
    interval = 5
    }

    order = ""
    order = "wireless wlp3s0"
    order += "disk /"
    order += "time"

    wireless wlp3s0 {
    format_up = "W: (%quality at %essid) %ip"
    format_down = "W: down"
    }

    disk "/" {
    format = "/ %free"
    }
  '';
}
