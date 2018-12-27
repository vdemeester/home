{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ../hardware/dell-latitude-e6540.nix ];
  time.timeZone = "Europe/Paris";
  fileSystems."/mnt/synodine" = {
    device = "192.168.12.19:/";
    fsType = "nfs";
    options = ["x-systemd.automount" "noauto"];
  };
  profiles = {
    avahi.enable = true;
    dev.enable = true;
    nix-config.buildCores = 4;
    ssh.enable = true;
    syncthing.enable = true;
  };
  networking = {
    enableIPv6 = false;
    firewall.allowedTCPPorts = [ 3389 2375 7946 9000 80 ];
    firewall.allowPing = true;
  };
  services = {
    logind.extraConfig = "HandleLidSwitch=ignore";
    syncthing-edge.guiAddress = "${wireguard.ips.honshu}:8384";
    wireguard = {
      enable = true;
      ips = [ "${wireguard.ips.honshu}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
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
      OnFailure = "status-email-root@%n.service";
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
      ExecStart = "${pkgs.nur.repos.vdemeester.ape}/bin/ape up /home/vincent/var/mirrors/";
      Environment = "PATH=/run/current-system/sw/bin/";
      OnFailure = "status-email-root@%n.service";
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
}
