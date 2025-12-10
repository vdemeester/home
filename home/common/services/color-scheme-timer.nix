{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.color-scheme-timer;
in
{
  options.services.color-scheme-timer = {
    enable = mkEnableOption "automatic color scheme switching based on time of day";

    latitude = mkOption {
      type = types.str;
      default = "48.87";
      description = "Latitude for calculating sunrise/sunset times";
    };

    longitude = mkOption {
      type = types.str;
      default = "2.33";
      description = "Longitude for calculating sunrise/sunset times";
    };

    lightTime = mkOption {
      type = types.str;
      default = "07:00";
      description = "Time to switch to light mode (HH:MM format)";
    };

    darkTime = mkOption {
      type = types.str;
      default = "19:00";
      description = "Time to switch to dark mode (HH:MM format)";
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.color-scheme-light = {
      Unit = {
        Description = "Switch to light color scheme";
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.toggle-color-scheme}/bin/toggle-color-scheme light";
      };
    };

    systemd.user.services.color-scheme-dark = {
      Unit = {
        Description = "Switch to dark color scheme";
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.toggle-color-scheme}/bin/toggle-color-scheme dark";
      };
    };

    systemd.user.timers.color-scheme-light = {
      Unit = {
        Description = "Timer for switching to light color scheme";
      };
      Timer = {
        OnCalendar = cfg.lightTime;
        Persistent = true;
      };
      Install = {
        WantedBy = [ "timers.target" ];
      };
    };

    systemd.user.timers.color-scheme-dark = {
      Unit = {
        Description = "Timer for switching to dark color scheme";
      };
      Timer = {
        OnCalendar = cfg.darkTime;
        Persistent = true;
      };
      Install = {
        WantedBy = [ "timers.target" ];
      };
    };
  };
}
