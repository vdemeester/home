{ config, nixosConfig, lib, pkgs, ... }:

let
  emacs-in-folder = pkgs.writeScript "emacs-in-folder" ''
    #!/usr/bin/env bash
    fd . -d 3 --type d ~/src | ${pkgs.wofi}/bin/wofi -dmenu | xargs -I {} zsh -i -c "cd {}; emacs ."
  '';
  fontConf = {
    names = [ "Ubuntu Mono" ];
    size = 12.0;
  };
in
{
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    systemdIntegration = true;
    extraSessionCommands = ''
      export SDL_VIDEODRIVER=wayland
      export QT_QPA_PLATFORM=wayland
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      export _JAVA_AWT_WM_NONREPARTENTING=1
      export MOZ_ENABLE_WAYLAND=1
    '';
    config = {
      gaps = {
        inner = 2;
        outer = 2;
      };
      modifier = "Mod4";
      terminal = "${pkgs.kitty}/bin/kitty";
      menu = "${pkgs.wofi}/bin/wofi -G --show drun -modi 'drun,run,window,ssh'";
      bindkeysToCode = true;
      input = {
        "type:keyboard" = {
          xkb_layout = "fr";
          xkb_variant = "bepo";
          xkb_options = "grp:menu_toggle,grp_led:caps,compose:caps";
        };
      };
      output = {
        "*" = {
          # { command = "${pkgs.swaybg}/bin/swaybg -i ~/desktop/pictures/lockscreen -m fill"; }
          bg = "~/desktop/pictures/lockscreen fill";
        };
      };
      fonts = fontConf;
      bars = [
        {
          command = "${pkgs.waybar}/bin/waybar";
          mode = "hide";
          position = "bottom";
          id = "mainBar";
        }
      ];
      keybindings =
        let
          mod = config.wayland.windowManager.sway.config.modifier;
          inherit (config.wayland.windowManager.sway.config) left down up right menu terminal;
        in
        {
          "${mod}+Return" = "exec ${terminal}";

          "${mod}+Shift+Return" = "exec emacsclient -c";
          "${mod}+Control+Return" = "exec emacs";
          "${mod}+Control+Shift+Return" = "exec ${emacs-in-folder}";

          # "${mod}+${left}" = "focus left";
          # "${mod}+${down}" = "focus down";
          # "${mod}+${up}" = "focus up";
          # "${mod}+${right}" = "focus right";

          "${mod}+Left" = "focus left";
          "${mod}+Down" = "focus down";
          "${mod}+Up" = "focus up";
          "${mod}+Right" = "focus right";

          # "${mod}+Shift+${left}" = "move left";
          # "${mod}+Shift+${down}" = "move down";
          # "${mod}+Shift+${up}" = "move up";
          # "${mod}+Shift+${right}" = "move right";

          "${mod}+Shift+Left" = "move left";
          "${mod}+Shift+Down" = "move down";
          "${mod}+Shift+Up" = "move up";
          "${mod}+Shift+Right" = "move right";

          "${mod}+Control+Left" = "workspace prev_on_output";
          "${mod}+Control+Down" = "workspace prev";
          "${mod}+Control+Up" = "workspace next";
          "${mod}+Control+Right" = "workspace next_on_output";

          "${mod}+Shift+Control+Left" = "move workspace to output left";
          "${mod}+Shift+Control+Down" = "move workspace to output down";
          "${mod}+Shift+Control+Up" = "move workspace to output up";
          "${mod}+Shift+Control+Right" = "move workspace to output right";

          # "${mod}+b" = "splith";
          # "${mod}+v" = "splitv";
          # "${mod}+f" = "fullscreen toggle";

          "${mod}+Shift+space" = "floating toggle";
          "${mod}+space" = "focus mode_toggle";

          #"${mod}+Shift+minus" = "move scratchpad";
          #"${mod}+minus" = "scratchpad show";

          "XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 10%+";
          "XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 10%-";
          "Shift+XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 1%+";
          "Shift+XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 1%-";
        };
      window.commands = [
        {
          command = "inhibit_idle fullscreen";
          criteria.app_id = "firefox";
        }
        {
          command = "inhibit_idle fullscreen";
          criteria.app_id = "mpv";
        }
        {
          # spotify doesn't set its WM_CLASS until it has mapped, so the assign is not reliable
          command = "move to workspace 10";
          criteria.class = "Spotify";
        }
        {
          command = "move to scratchpad, scratchpad show";
          criteria = {
            app_id = "metask";
          };
        }
        {
          command = "move to scratchpad, scratchpad show";
          criteria = {
            app_id = "emacs";
            title = "^_emacs scratchpad_$";
          };
        }
        {
          criteria = { title = "Save File"; };
          command = "floating enable, resize set width 600px height 800px";
        }
        {
          criteria = { class = "pavucontrol"; };
          command = "floating enable";
        }
        {
          criteria = { title = "(Sharing Indicator)"; };
          command = "inhibit_idle visible, floating enable";
        }
        {
          # browser zoom|meet|bluejeans
          criteria = { title = "(Blue Jeans)|(Meet)|(Zoom Meeting)"; };
          command = "inhibit_idle visible";
        }
        # for_window [app_id="^chrome-.*"] shortcuts_inhibitor disable
        {
          criteria = { app_id = "^chrome-.*"; };
          command = "shortcuts_inhibitor disable";
        }
      ];
      startup = [
        { command = "dbus-update-activation-environment --systemd WAYLAND_DISPLAY DISPLAY DBUS_SESSION_BUS_ADDRESS SWAYSOCK XDG_SESSION_TYPE XDG_SESSION_DESKTOP XDG_CURRENT_DESKTOP"; } #workaround
        { command = "${pkgs.networkmanagerapplet}/bin/nm-applet --indicator"; }
        # { command = "systemctl --user restart waybar"; always = true; }
        { command = "systemctl --user restart kanshi"; always = true; }
        # { command = "${pkgs.kitty}/bin/kitty --title metask --class metask tmux"; }
        # { command = ''emacsclient -n -c -F "((name . \"_emacs scratchpad_\"))''; }
      ];
    };
    extraConfig =
      let
        mod = config.wayland.windowManager.sway.config.modifier;
        inherit (config.wayland.windowManager.sway.config) left down up right menu terminal;
      in
      ''
        bindcode ${mod}+33 exec "${menu}"
        bindcode ${mod}+Control+33 exec "${pkgs.wofi-emoji}/bin/wofi-emoji -G"
        bindcode ${mod}+Shift+24 kill
        bindcode ${mod}+38 focus parent
        bindcode ${mod}+39 layout stacking
        bindcode ${mod}+25 layout tabbed
        bindcode ${mod}+26 layout toggle split
        bindcode ${mod}+Shift+54 reload
        bindcode ${mod}+Shift+26 exec "swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' 'swaymsg exit'"
        bindcode ${mod}+32 mode resize
        bindcode ${mod}+Shift+32 exec "${pkgs.swaylock}/bin/swaylock -i $HOME/desktop/pictures/lockscreen"

        # switch to workspace
        bindcode ${mod}+10 workspace number 1
        bindcode ${mod}+11 workspace number 2
        bindcode ${mod}+12 workspace number 3
        bindcode ${mod}+13 workspace number 4
        bindcode ${mod}+14 workspace number 5
        bindcode ${mod}+15 workspace number 6
        bindcode ${mod}+16 workspace number 7
        bindcode ${mod}+17 workspace number 8
        bindcode ${mod}+18 workspace number 9
        bindcode ${mod}+19 workspace number 10

        # move focused container to workspace
        bindcode ${mod}+Shift+10 move container to workspace number 1
        bindcode ${mod}+Shift+11 move container to workspace number 2
        bindcode ${mod}+Shift+12 move container to workspace number 3
        bindcode ${mod}+Shift+13 move container to workspace number 4
        bindcode ${mod}+Shift+14 move container to workspace number 5
        bindcode ${mod}+Shift+15 move container to workspace number 6
        bindcode ${mod}+Shift+16 move container to workspace number 7
        bindcode ${mod}+Shift+17 move container to workspace number 8
        bindcode ${mod}+Shift+18 move container to workspace number 9
        bindcode ${mod}+Shift+19 move container to workspace number 10

        bindcode ${mod}+Control+39 split h
        bindcode ${mod}+41 fullscreen toggle

        bindsym XF86AudioRaiseVolume exec ${pkgs.pamixer}/bin/pamixer -ui 5 && ${pkgs.pamixer}/bin/pamixer --get-volume > $SWAYSOCK.wob
        bindsym XF86AudioLowerVolume exec ${pkgs.pamixer}/bin/pamixer -ud 5 && ${pkgs.pamixer}/bin/pamixer --get-volume > $SWAYSOCK.wob
        bindsym XF86AudioMute exec ${pkgs.pamixer}/bin/pamixer --toggle-mute && ( ${pkgs.pamixer}/bin/pamixer --get-mute && echo 0 > $SWAYSOCK.wob ) || ${pkgs.pamixer}/bin/pamixer --get-volume > $SWAYSOCK.wob

        bindsym XF86AudioPlay exec "playerctl play-pause"
        bindsym XF86Messenger exec "playerctl play-pause"
        bindsym XF86AudioNext exec "playerctl next"
        bindsym XF86Go exec "playerctl next"
        bindsym XF86AudioPrev exec "playerctl previous"
        bindsym Cancel exec "playerctl previous"

        bindcode ${mod}+49 exec swaymsg [app_id="metask"] scratchpad show || exec ${pkgs.kitty}/bin/kitty --title metask --class metask tmux
        bindsym --whole-window button8 exec sswaymsg [app_id="metask"] scratchpad show || exec ${pkgs.kitty}/bin/kitty --title metask --class metask tmux
        bindcode ${mod}+Shift+49 exec swaymsg '[app_id="emacs" title="^_emacs scratchpad_$"]' scratchpad show || exec ${config.programs.emacs.package}/bin/emacsclient -c -F "((name . \"_emacs scratchpad_\"))"
        bindsym --whole-window button9 exec swaymsg '[app_id="emacs" title="^_emacs scratchpad_$"]' scratchpad show || exec ${config.programs.emacs.package}/bin/emacsclient -c -F "((name . \"_emacs scratchpad_\"))"

        # Mouse
        # Disabled as it doesn't play well with thinkpad's trackpoint :D
        # bindsym --whole-window button6 workspace next_on_output
        # bindsym --whole-window button7 workspace prev_on_output

        bindsym ${mod}+F10 exec ${pkgs.my.scripts}/bin/shot %d
        bindsym ${mod}+Shift+F10 exec ${pkgs.my.scripts}/bin/shotf %d

        bindsym ${mod}+F9 exec ${pkgs.mako}/bin/makoctl mode -s do-not-disturb
        bindsym ${mod}+Shift+F9 exec ${pkgs.mako}/bin/makoctl mode -s default
      '';
  };
  programs = {
    waybar = {
      enable = true;
      # systemd.enable = true;
      style = ./waybar.css;
      settings = [{
        layer = "bottom";
        position = "bottom";
        mode = "hide";
        modules-left = [ "sway/workspaces" "sway/mode" "custom/media" ];
        modules-center = [ "clock" "custom/notification" ];
        modules-right = [ "temperature" "pulseaudio" "backlight" "battery#bat0" "tray" ];
        ipc = true;
        id = "mainBar";
        "clock" = {
          "interval" = 30;
          "format" = "{:<b>%H:%M</b>}";
          "tooltip-format" = "<big><b>{:%Y %B}</b></big>\n{calendar}";
          "format-alt" = "{:%A %d %B %Y}";
        };
        "battery#bat0" = {
          "bat" = "BAT0";
          "states" = {
            "warning" = 30;
            "critical" = 15;
          };
          "format" = "{icon} {capacity}";
          "format-charging" = " {capacity}";
          "format-plugged" = "";
          "format-alt" = "{icon} {time}";
          "format-full" = "";
          "format-icons" = [ "" "" "" "" "" ];
        };
        "custom/notification" = {
          "tooltip" = false;
          "format" = "{icon} ";
          "format-icons" = {
            "notification" = "<span foreground='red'></span>";
            "none" = "";
            "dnd-notification" = "<sup></sup>";
            "dnd-none" = "";
          };
          "return-type" = "json";
          # "exec-if" = "which swaync-client";
          "exec" = "swaync-client -swb";
          "on-click" = "swaync-client -t -sw";
          "on-click-right" = "swaync-client -d -sw";
          "escape" = true;
        };
        "pulseaudio" = {
          "format" = "{icon} {volume:2}% ";
          "format-bluetooth" = "{icon}  {volume}% ";
          "format-muted" = " ";
          "format-icons" = {
            "phone" = [
              " "
              " "
              " "
              " "
            ];
            "default" = [
              ""
              ""
              ""
            ];
          };
          "scroll-step" = 10;
          "on-click-right" = "${pkgs.pavucontrol}/bin/pavucontrol";
          "on-click" = "${pkgs.pamixer}/bin/pamixer -t";
        };
        "backlight" = {
          "format" = "{icon} {percent}% ";
          "format-icons" = [
            ""
            ""
          ];
        };
      }];
    };
    mako = {
      enable = true;
      font = "Ubuntu Mono 12";
      defaultTimeout = 5000; # 5s timeout
      groupBy = "app-name,summary";
      extraConfig = ''
        on-button-left=dismiss
        on-button-middle=invoke-default-action
        on-button-right=dismiss
        
        [mode=do-not-disturb]
        invisible=1
      '';
    };
    kitty = {
      enable = true;
      settings = {
        term = "xterm-256color";
        close_on_child_death = "yes";
        font_family = "Ubuntu Mono";
      };
      theme = "Tango Light";
    };
  };
  services = {
    blueman-applet.enable = nixosConfig.modules.hardware.bluetooth.enable;
    pasystray.enable = nixosConfig.modules.hardware.audio.enable;
    udiskie.enable = true;
    # network-manager-applet.enable = true;
    gammastep = {
      enable = true;
      provider = "geoclue2";
      # longitude = "2.333333";
      # latitude = "48.866667";
    };
    kanshi = {
      enable = true;
      profiles = {
        "home-undocked" = {
          outputs = [
            # Output eDP-1 'AU Optronics 0xD291 Unknown'
            { criteria = "eDP-1"; status = "enable"; position = "0,0"; mode = "1920x1200"; scale = 1.0; }
          ];
        };
        "home-docked" = {
          outputs = [
            # Old: Output eDP-1 'AU Optronics 0xD291 Unknown'
            # Output eDP-1 'Unknown 0xD291 Unknown'
            # Output DP-5 'LG Electronics LG ULTRAWIDE 0x0005D10C' (focused)
            # { criteria = "LG Electronics LG ULTRAWIDE 0x0000D50C"; status = "enable"; position = "0,0"; mode = "3440x1440"; scale = 1.0; }
            { criteria = "DP-5"; status = "enable"; position = "0,0"; mode = "3440x1440"; scale = 1.0; }
            # Use it as a "shareable" screen when needed
            { criteria = "eDP-1"; status = "enable"; position = "1460,1440"; mode = "1920x1200"; scale = 1.0; }
          ];
        };
      };
    };
    swayidle = {
      enable = true;
      events = [
        { event = "before-sleep"; command = "${pkgs.swaylock}/bin/swaylock --daemonize -i $HOME/desktop/pictures/lockscreen"; }
        { event = "lock"; command = "${pkgs.swaylock}/bin/swaylock --daemonize -i $HOME/desktop/pictures/lockscreen"; }
      ];
      timeouts = [
        { timeout = 600; command = "${pkgs.swaylock}/bin/swaylock --daemonize -i $HOME/desktop/pictures/lockscreen"; }
        {
          timeout = 1200;
          command = ''${pkgs.sway}/bin/swaymsg "output * dpms off"'';
          resumeCommand = ''${pkgs.sway}/bin/swaymsg "output * dpms on"'';
        }
      ];
    };
  };
  home.packages = with pkgs; [
    swaylock
    swayidle
    swaybg
    wf-recorder
    wl-clipboard
    wtype
    mako
    swaynotificationcenter
    wofi
    waybar
    slurp
    grim
    gnome.zenity
    clipman
    # terminals
    # FIXME move this away, they work on both Xorg and Wayland/Sway
    alacritty
  ];

}

