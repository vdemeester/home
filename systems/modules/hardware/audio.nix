{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf mkMerge mkOption types;
  cfg = config.modules.hardware.audio;
in
{
  options.modules.hardware.audio = {
    enable = mkEnableOption "enable audio";
    pulseaudio = {
      enable = mkEnableOption "enable pulseaudio";
      tcp = mkOption {
        default = false;
        description = "enable pulseaudio tcp";
        type = types.bool;
      };
    };
    pipewire = {
      enable = mkEnableOption "enable pipewire";
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      # Enable sound (alsa)
      sound.enable = true;
      # FIXME is it needed
      security.pam.loginLimits = [
        { domain = "@audio"; item = "memlock"; type = "-"; value = "unlimited"; }
        { domain = "@audio"; item = "rtprio"; type = "-"; value = "99"; }
        { domain = "@audio"; item = "nofile"; type = "-"; value = "99999"; }
      ];
    }
    (mkIf cfg.pipewire.enable {
      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
        wireplumber.enable = true;
      };
      environment.etc."pipewire/pipewire.conf.d/raop-discover.conf".text = ''
        context.modules = [
           {
               name = libpipewire-module-raop-discover
               args = { }
           }
        ]
      '';
      networking.firewall = {
        allowedTCPPorts = [ 6001 6002 ];
      };
      environment.etc."pipewire/pipewire-pulse.conf.d/50-network-party.conf".text = ''
        context.exec = [
            { path = "pactl" args = "load-module module-native-protocol-tcp" }
            { path = "pactl" args = "load-module module-zeroconf-discover" }
            { path = "pactl" args = "load-module module-zeroconf-publish" }
        ]
      '';
    })
    (mkIf cfg.pulseaudio.enable {
      # Enable and configure pulseaudio
      hardware.pulseaudio = {
        enable = true;
        support32Bit = true;
        zeroconf.discovery.enable = true;
      };
    })
    (mkIf (cfg.pulseaudio.enable || cfg.pipewire.enable) {
      # Add extra packages
      environment.systemPackages = with pkgs; [
        apulse # allow alsa application to use pulse
        pavucontrol # pulseaudio volume control
        pasystray # systray application
      ];
    })
    (mkIf (cfg.pulseaudio.enable && cfg.pulseaudio.tcp) {
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
