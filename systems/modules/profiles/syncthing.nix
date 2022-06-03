{ config, lib, pkgs, ... }:

with lib;
let
  unstable = versionOlder config.system.nixos.release "21.05";
  cfg = config.profiles.syncthing;
  isCurrentHost = n: v: n != config.networking.hostName;
  devices = {
    aomi = {
      id = "WAD7GYV-RXIL3V3-OT5PFZH-NRQHZWV-D3TGJVR-G4IANXZ-HTO5VT7-XE2WIQQ";
      addresses = [ "tcp://aomi.vpn" "tcp://aomi.home" ];
    };
    naruhodo = {
      id = "BKZN3FH-KRP4XRN-XFEVCCG-VANAUJN-YFAUS5Q-WUOLQQ7-II7I6PR-NVJZXQT";
      addresses = [ "tcp://naruhodo.vpn" "tcp://naruhodo.home" ];
      # addresses = [ "tcp://192.168.1.2" "tcp://void.home" ];
    };
    sakhalin = {
      id = "4TYYG7V-A67D5SN-HMEJCI7-POOZRLL-RNCIE4U-ZYVGTOB-JQ5DOSV-ZCGWUAL";
      addresses = [ "tcp://sakhalin.home" "tcp://sakhalin.vpn" ];
    };
    shikoku = {
      id = "KZMMXRR-UINDQTS-H3TV2W7-EIGOUDI-3LW4ZDG-7PRKDFV-MJ5KUTJ-YG5Y5AI";
      addresses = [ "tcp://shikoku.home" "tcp://shikoku.vpn" ];
    };
    okinawa = {
      id = "2RWT47Z-UGSH4QO-G4W6XN7-3XY722R-ZKGDN5U-4MDGHMA-6SM26QM-7VCQIAZ";
      addresses = [ "tcp://okinawa.home" "tcp://okinawa.vpn" ];
    };
    # Deprecated
    wakasu = {
      id = "XNCACMA-LMIZPRZ-J6LEMR5-BVI7IVQ-6HWWMUU-QUCA63X-ZE32NOP-QFDDGQM";
      addresses = [ "tcp://wakasu.home" "tcp://wakasu.vpn" ];
    };
  };
  deviceNames = builtins.attrNames (filterAttrs isCurrentHost devices);
in
{
  options = {
    profiles.syncthing = {
      enable = mkEnableOption "Enable syncthing profile";
    };
  };
  config = mkIf cfg.enable {
    services.syncthing =
      if (builtins.hasAttr "devices" config.services.syncthing)
      then {
        enable = true;
        user = "vincent";
        dataDir = "/home/vincent/.syncthing";
        configDir = "/home/vincent/.syncthing";
        devices = filterAttrs isCurrentHost devices;
        folders = {
          "/home/vincent/sync" = {
            label = "sync";
            id = "7dshg-r8zr6";
            devices = deviceNames;
          };
          "/home/vincent/desktop/org" = {
            label = "org";
            id = "sjpsr-xfwdu";
            devices = deviceNames;
          };
          "/home/vincent/desktop/documents" = {
            label = "documents";
            id = "oftdb-t5anv";
            devices = deviceNames;
          };
          "/home/vincent/desktop/pictures/screenshots" = {
            label = "screenshots";
            id = "prpsz-azlz9";
            devices = deviceNames;
          };
          "/home/vincent/desktop/pictures/wallpapers" = {
            label = "wallpapers";
            id = "wpiah-ydwwx";
            devices = deviceNames;
          };
        };
      }
      else {
        enable = true;
        user = "vincent";
        dataDir = "/home/vincent/.syncthing";
        configDir = "/home/vincent/.syncthing";
      };
  };
}
