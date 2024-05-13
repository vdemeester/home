{ config, lib, pkgs, nixosConfig, ... }:

{
  home.pointerCursor = {
    gtk.enable = true;
    x11.enable = true;
    package = pkgs.qogir-icon-theme;
    name = "Qogir";
    size = 24;
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
        modules-left = [ "sway/workspaces" "hyprland/workspaces" "sway/mode" "custom/media" ];
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
    kitty = {
      enable = true;
      # shellIntegration.enableZshIntegration = true;
      settings = {
        term = "xterm-256color";
        close_on_child_death = "yes";
        font_family = "Ubuntu Mono";
        tab_bar_edge = "top";
        listen_on = "unix:/tmp/my kitty";
        allow_remote_control = "yes";
        macos_option_as_alt = "yes";
        copy_on_select = "yes";

        active_tab_foreground = "#C6A0F6";
        active_tab_background = "#0c0c0c";
        inactive_tab_foreground = "#6E738D";
        inactive_tab_background = "#0c0c0c";
      };
      font = {
        name = "Ubuntu Mono";
        size = 12;
      };
      keybindings = {
        "shift+left" = "neighboring_window left";
        "shift+right" = "neighboring_window right";
        "shift+up" = "neighboring_window up";
        "shift+down" = "neighboring_window down";
      };
      theme = "Tango Light";
      # action_alias mkh kitten hints --alphabet asdfghjklqwertyuiopzxcvbnmASDFGHJKLQWERTYUIOPZXCVBNM 
      # map kitty_mod+n    mkh --type=linenum emacsclient -c -nw +{line} {path}
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
      settings = [
        {
          profile.name = "home-undocked";
          profile.outputs = [
            # Output eDP-1 'AU Optronics 0xD291 Unknown'
            { criteria = "eDP-1"; status = "enable"; position = "0,0"; mode = "1920x1200"; scale = 1.0; }
          ];
        }
        {
          profile.name = "home-docked";
          profile.outputs = [
            # Old: Output eDP-1 'AU Optronics 0xD291 Unknown'
            # Output eDP-1 'Unknown 0xD291 Unknown'
            # Output DP-5 'LG Electronics LG ULTRAWIDE 0x0005D10C' (focused)
            # { criteria = "LG Electronics LG ULTRAWIDE 0x0000D50C"; status = "enable"; position = "0,0"; mode = "3440x1440"; scale = 1.0; }
            { criteria = "DP-5"; status = "enable"; position = "0,0"; mode = "3440x1440"; scale = 1.0; }
            # Use it as a "shareable" screen when needed
            { criteria = "eDP-1"; status = "enable"; position = "1460,1440"; mode = "1920x1200"; scale = 1.0; }
          ];
        }
      ];
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
    swayidle = {
      enable = false;
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
    wf-recorder
    wl-clipboard
    wtype
    mako
    wofi
    waybar
    slurp
    grim
    gnome.zenity
    clipman
    qogir-icon-theme
    cliphist
  ];
}
