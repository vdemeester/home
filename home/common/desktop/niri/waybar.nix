_: {
  programs.waybar = {
    enable = true;
    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        "modules-left" = [
          "tray"
          "custom/separator"
          # "niri/workspaces"
          # "custom/agenda"
        ];
        "modules-center" = [
          "clock"
        ];
        "modules-right" = [
          "niri/language"
          "custom/separator"
          "wireplumber"
          "custom/separator"
          "battery"
        ];
        "custom/separator" = {
          "format" = "|";
          "interval" = "once";
          "tooltip" = false;
        };
        "clock" = {
          "format" = "{:%a %d %b %H:%M}";
          "tooltip-format" = "<tt><small>{calendar}</small></tt>";
          "timezones" = [
            ""
            "Asia/Calcutta"
          ];
          # "on-click" = "~/.local/share/chmouzies/waybar/clockclik.sh && wm-jumpapp kitty";
          "calendar" = {
            "mode" = "month";
            "mode-mon-col" = 3;
            "on-scroll" = 1;
            "on-click-right" = "mode";
            "format" = {
              "months" = "<span color='yellow'><big>{}</big></span>";
              "days" = "<span color='#fff'><b>{}</b></span>";
              "weeks" = "<span color='#99ffdd'><b>W{}</b></span>";
              "weekdays" = "<span color='white'><i><u>{}</u></i></span>";
              "today" = "<span color='#ff6699'><b><u>{}</u></b></span>";
            };
          };
          "actions" = {
            "on-click-right" = "mode";
            "on-click-forward" = "tz_up";
            "on-click-backward" = "tz_down";
            "on-scroll-up" = "shift_down";
            "on-scroll-down" = "shift_up";
          };
        };
        "battery" = {
          "states" = {
            "good" = 75;
            "warning" = 30;
            "critical" = 15;
          };
          "format-charging" = "🔌 {capacity}% ";
          "format-good" = "{icon} {capacity}%";
          "format" = "{icon} {capacity}% ";
          "format-icons" = [
            ""
            ""
            ""
            ""
            "  "
          ];
        };
        "tray" = {
          "icon-size" = 16;
          "spacing" = 12;
        };
        "wireplumber" = {
          "format" = " {node_name} {volume:2}% ";
          "format-muted" = " {volume:2}% ";
          "scroll-step" = 5;
          "on-right" = "pwvucontrol";
        };
        # custom/wg (wireguard status)
        # custom/vpn (rh vpn)
        "niri/window" = {
          "format" = "{}";
          "rewrite" = {
            "(.*) - Mozilla Firefox" = "🌎 $1";
            "(.*) - zsh" = "> [$1]";
          };
        };
        "niri/language" = {
          "format" = "{short}-{variant}";
          "on-click-right" = "niri msg action switch-layout prev";
          "on-click" = "niri msg action switch-layout next";
        };
      };
    };
    style = ''
      * {
        font-size: 16px;
        font-family: "JetBrains Mono";
        border: none;
        border-radius: 0;
        min-height: 0px;
      }

      window#waybar {
        background: transparent;
        padding: 5px;
        color: #fff;
      }

      #workspaces {
        margin: 0 4px;
      }

      /* If workspaces is the leftmost module, omit left margin */
      .modules-left > widget:first-child > #workspaces {
        margin-left: 0px;
      }

      /* If workspaces is the rightmost module, omit right margin */
      .modules-right > widget:last-child > #workspaces {
        margin-right: 2px;
      }

      #workspaces button {
        color: #4f4f4f;
      }

      #workspaces button:hover {
        background: rgba(120, 136, 148, 0.7);
        box-shadow: inherit;
        text-shadow: inherit;
      }

      #workspaces button.focused {
        color: #9c6ace;
      }

      button:hover {
        background: inherit;
        box-shadow: inset 0 -3px #000;
      }

      #custom-vpn.connected {
        color: #ff5fff;
      }

      #custom-email-personal {
        color: #00008b;
      }

      #custom-email-work {
        color: #eb4d4b;
      }

      #clock,
      #battery,
      #cpu,
      #memory,
      #disk,
      #temperature,
      #backlight,
      #network,
      #pulseaudio,
      #custom-media,
      #tray,
      #mode,
      #idle_inhibitor,
      #scratchpad,
      #custom-email-personal,
      #custom-email-work,
      #custom-github_notifications,
      #custom-media,
      #custom-power,
      #custom-vpn,
      #custom-space,
      #custom-weather,
      #custom-mynetwork,
      #custom-arch_updates,
      #mpd {
        padding: 0 2px;
      }

      #clock {
        font-weight: 500;
      }

      #custom-agenda.soon {
        color: #ca907e;
      }

      #custom-agenda {
        color: #696969;
      }

      #custom-agenda.current {
        color: #7393b3;
        font-style: oblique;
      }

      #custom-github-prs.has-prs {
        color: #8e24aa; /* Accessible purple for white backgrounds */ /* Mauve in Catppuccin */
      }

      #custom-separator {
        margin: 0 1px;
      }
      		'';
  };
}
