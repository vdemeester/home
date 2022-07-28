{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf mkMerge versionOlder;
  cfg = config.modules.hardware.bluetooth;
  stable = versionOlder config.system.nixos.release "21.05";
in
{
  options.modules.hardware.bluetooth = {
    enable = mkEnableOption "Enable bluetooth";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      hardware.bluetooth.enable = true;
      # warnings = if stable then [ ] else [ "NixOS release: ${config.system.nixos.release}" ];
    }
    (mkIf config.modules.hardware.audio.enable {
      hardware.pulseaudio = {
        # NixOS allows either a lightweight build (default) or full build of
        # PulseAudio to be installed.  Only the full build has Bluetooth
        # support, so it must be selected here.
        package = pkgs.pulseaudioFull;
      };
    })
    (mkIf (stable && config.modules.hardware.audio.enable) {
      hardware.bluetooth.extraConfig = ''
        [General]
        Enable=Source,Sink,Media,Socket
      '';
    })
    (mkIf ((!stable) && config.modules.hardware.audio.enable)
      {
        hardware.bluetooth.settings = {
          General = {
            Enable = "Source,Sink,Media,Socket";
          };
        };
      })
  ]);
}
