{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf mkMerge mkOption types;
  cfg = config.modules.hardware.audio;
in
{
  options.modules.hardware.audio = {
    enable = mkEnableOption "enable audio";
    tcp = mkOption {
      default = false;
      description = "enable pulseaudio tcp";
      type = types.bool;
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
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
    }
    (mkIf cfg.tcp {
      hardware.pulseaudio = {
        zeroconf = {
          discovery.enable = cfg.tcp;
          publish.enable = cfg.tcp;
        };
        tcp = {
          enable = cfg.tcp;
          anonymousClients = {
            allowAll = true;
            allowedIpRanges = [ "127.0.0.1" "192.168.12.0/24" "10.0.0.0/24" ];
          };
        };
      };
      networking.firewall = {
        allowedTCPPorts = [ 4713 ];
      };
    })
  ]);
}
