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
      menu = "${pkgs.wofi}/bin/wofi --show drun -modi 'drun,run,window,ssh'";
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
          "${mod}+p" = "exec ${menu}";
          "${mod}+Control+p" = "exec ${pkgs.wofi-emoji}/bin/wofi-emoji";
          "${mod}+Return" = "exec ${terminal}";

          "${mod}+Shift+q" = "kill";

          "${mod}+Shift+Return" = "exec emacsclient -c";
          "${mod}+Control+Return" = "exec emacs";
          "${mod}+Control+Shift+Return" = "exec ${emacs-in-folder}";

          "${mod}+${left}" = "focus left";
          "${mod}+${down}" = "focus down";
          "${mod}+${up}" = "focus up";
          "${mod}+${right}" = "focus right";

          "${mod}+Left" = "focus left";
          "${mod}+Down" = "focus down";
          "${mod}+Up" = "focus up";
          "${mod}+Right" = "focus right";

          "${mod}+Shift+${left}" = "move left";
          "${mod}+Shift+${down}" = "move down";
          "${mod}+Shift+${up}" = "move up";
          "${mod}+Shift+${right}" = "move right";

          "${mod}+Shift+Left" = "move left";
          "${mod}+Shift+Down" = "move down";
          "${mod}+Shift+Up" = "move up";
          "${mod}+Shift+Right" = "move right";

          "${mod}+Mod1+Left" = "workspace prev_on_output";
          # "${mod}+Mod1+Down" = "move down";
          # "${mod}+Mod1+Up" = "move up";
          "${mod}+Mod1+Right" = "workspace next_on_output";

          # "${mod}+b" = "splith";
          # "${mod}+v" = "splitv";
          "${mod}+f" = "fullscreen toggle";
          "${mod}+a" = "focus parent";

          "${mod}+s" = "layout stacking";
          "${mod}+w" = "layout tabbed";
          "${mod}+e" = "layout toggle split";

          "${mod}+Shift+space" = "floating toggle";
          "${mod}+space" = "focus mode_toggle";

          "${mod}+Shift+minus" = "move scratchpad";
          "${mod}+minus" = "scratchpad show";

          "${mod}+Shift+c" = "reload";
          "${mod}+Shift+e" =
            "exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' 'swaymsg exit'";

          "${mod}+o" = "mode resize";

          "${mod}+Shift+o" = "exec ${pkgs.swaylock}/bin/swaylock -i $HOME/desktop/pictures/lockscreen";

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
          command = "move to scratchpad";
          criteria = {
            app_id = "kitty";
            class = "metask";
          };
        }
      ];
      startup = [
        { command = "mako"; }
        { command = "${pkgs.networkmanagerapplet}/bin/nm-applet --indicator"; }
        { command = "systemctl --user restart waybar"; always = true; }
        { command = "${pkgs.kitty}/bin/kitty --title metask --class metask tmux"; }
        { command = ''emacsclient -n -c -F "((name . \"_emacs scratchpad_\"))''; }
      ];
    };
    extraConfig =
      let
        mod = config.wayland.windowManager.sway.config.modifier;
      in
      ''
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
        bindcode ${mod}+Shift+19 move container to workspace number 0

        bindcode ${mod}+Control+39 split h

        bindsym XF86AudioRaiseVolume exec ${pkgs.pamixer}/bin/pamixer -ui 5 && ${pkgs.pamixer}/bin/pamixer --get-volume > $SWAYSOCK.wob
        bindsym XF86AudioLowerVolume exec ${pkgs.pamixer}/bin/pamixer -ud 5 && ${pkgs.pamixer}/bin/pamixer --get-volume > $SWAYSOCK.wob
        bindsym XF86AudioMute exec ${pkgs.pamixer}/bin/pamixer --toggle-mute && ( ${pkgs.pamixer}/bin/pamixer --get-mute && echo 0 > $SWAYSOCK.wob ) || ${pkgs.pamixer}/bin/pamixer --get-volume > $SWAYSOCK.wob

      '';
  };
  programs = {
    waybar = {
      enable = true;
      systemd.enable = true;
      settings = [{
        layer = "bottom";
        position = "bottom";
        mode = "hide";
        modules-left = [ "sway/workspaces" "sway/mode" "custom/media" ];
        modules-center = [ "sway/window" ];
        modules-right = [ "idle_inhibitor" "pulseaudio" "temperature" "backlight" "sway/language" "battery" "clock" "tray" ];
        ipc = true;
        id = "mainBar";
      }];
    };
    mako = {
      enable = true;
      font = "Ubuntu Mono 12";
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
    network-manager-applet.enable = true;
    kanshi.enable = true;
    swayidle = {
      enable = true;
      events = [
        { event = "before-sleep"; command = "${pkgs.swaylock}/bin/swaylock -i $HOME/desktop/pictures/lockscreen"; }
        { event = "after-resume"; command = ''swaymsg "output * dpms on"''; }
        { event = "lock"; command = "${pkgs.swaylock}/bin/swaylock -i $HOME/desktop/pictures/lockscreen"; }
        { event = "unlock"; command = ''swaymsg "output * dpms on"''; }
      ];
      timeouts = [
        { timeout = 600; command = "${pkgs.swaylock}/bin/swaylock -i $HOME/desktop/pictures/lockscreen"; }
        { timeout = 1200; command = ''swaymsg "output * dpms off"''; }
      ];
    };
  };
  home.packages = with pkgs; [
    swaylock
    swayidle
    swaybg
    wl-clipboard
    mako
    wofi
    waybar
    # terminals
    # FIXME move this away, they work on both Xorg and Wayland/Sway
    alacritty
  ];
}

