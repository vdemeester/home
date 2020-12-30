{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.hardware.audio;
in
{
  options.modules.hardware.audio = {
    enable = mkEnableOption "enable audio";
  };
  config = mkIf cfg.enable {
    # Add extra packages
    environment.systemPackages = with pkgs; [
      apulse # allow alsa application to use pulse
      pavucontrol # pulseaudio volume control
      pasystray # systray application
    ];
    # Enable sound (alsa)
    sound.enable = true;
    # Enable and configure pulseaudio
    hardware.pulseaudio = {
      enable = true;
      support32Bit = true;
    };
    # FIXME is it needed
    security.pam.loginLimits = [
      { domain = "@audio"; item = "memlock"; type = "-"; value = "unlimited"; }
      { domain = "@audio"; item = "rtprio"; type = "-"; value = "99"; }
      { domain = "@audio"; item = "nofile"; type = "-"; value = "99999"; }
    ];
  };
}
