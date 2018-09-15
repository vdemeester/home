{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
    ../hardware-configuration.nix
    ../hardware/dell-latitude-e6540.nix
    ../profiles/server.nix
    ../profiles/dev.nix
    ../profiles/fish.nix
    ../profiles/avahi.nix
    ../profiles/containerd.nix
    ../profiles/syncthing.nix
    ../service/ssh-tunnel.nix
    ../location/home.nix
  ];

  time.timeZone = "Europe/Paris";

  services = {
    logind.extraConfig = "HandleLidSwitch=ignore";
    ssh-tunnel = {
      enable = true;
      localUser = "vincent";
      remoteHostname = "95.85.58.158";
      remotePort = 22;
      remoteUser = "vincent";
      bindPort = 2224;
    };
  };

  environment.etc."vrsync".text = ''
/home/vincent/desktop/pictures/screenshots/ vincent@synodine.local:/volumeUSB2/usbshare/pictures/screenshots/
/home/vincent/desktop/pictures/wallpapers/ vincent@synodine.local:/volumeUSB2/usbshare/pictures/wallpapers/
/home/vincent/desktop/pictures/photos/ vincent@synodine.local:/volumeUSB2/usbshare/pictures/photos/
/home/vincent/desktop/documents/ vincent@synodine.local:/volume1/documents/
/run/media/vincent/FcCuir/music/ vincent@synodine.local:/volumeUSB2/usbshare/music/
vincent@synodine.local:/volume1/backup/drive/ /run/media/vincent/Toshito/backup/drive/
  '';

  systemd.services.vrsync = {
    description = "vrsync - sync folders to NAS";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.vrsync}/bin/vrsync";
      Environment = "PATH=/run/current-system/sw/bin";
    };
  };
  systemd.timers.vrsync = {
    description = "vrsync daily";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "daily";
      Persistent = "true";
    };
  };
  systemd.timers.vrsync.enable = true;

  # ape – sync git mirrors
  systemd.services.ape = {
    description = "Ape - sync git mirrors";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      User = "vincent";
      ExecStart = "${pkgs.ape}/bin/ape up /home/vincent/share/mirrors/";
      Environment = "PATH=/run/current-system/sw/bin/";
    };
  };
  systemd.timers.ape = {
    description = "Ape hourly";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "hourly";
      Persistent = "true";
    };
  };
  systemd.timers.ape.enable = true;

  networking.enableIPv6 = false;
  networking.firewall.allowedTCPPorts = [ 3389 2375 7946 9000 ];
}
