{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ../hardware/dell-latitude-e6540.nix ./home.nix ];
  networking = {
    firewall.enable = false; # we are in safe territory :D
    bridges.br1.interfaces = [ "eno1" ];
    interfaces.eno1 = {
      useDHCP = true;
    };
  };
  profiles = {
    avahi.enable = true;
    dev.enable = true;
    nix-config.buildCores = 4;
    ssh.enable = true;
    syncthing.enable = true;
    virtualization = {
      enable = true;
      nested = true;
      listenTCP = true;
    };
  };
  services = {
    logind.lidSwitch = "ignore";
    syncthing.guiAddress = "0.0.0.0:8384";
    wireguard = {
      enable = true;
      ips = [ "${wireguard.ips.honshu}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };
  };
  # -----------------------------------
  environment.etc."vrsync".text = ''
/home/vincent/desktop/pictures/screenshots/ vincent@synodine.home:/volumeUSB2/usbshare/pictures/screenshots/
/home/vincent/desktop/pictures/wallpapers/ vincent@synodine.home:/volumeUSB2/usbshare/pictures/wallpapers/
/home/vincent/desktop/documents/ vincent@synodine.home:/volume1/documents/
/mnt/Toshito/photos/ vincent@synodine.home:/volumeUSB2/usbshare/pictures/photos/
/mnt/Toshito/music/ vincent@synodine.home:/volumeUSB2/usbshare/music/
  '';
  systemd.services.vrsync = {
    description = "vrsync - sync folders to NAS";
    requires = [ "network-online.target" ];
    after    = [ "network-online.target" ];

    unitConfig.X-StopOnRemoval = false;
    restartIfChanged = false;

    path = with pkgs; [ rsync coreutils bash openssh ];
    script = ''
    ${pkgs.vrsync}/bin/vrsync
    '';

    startAt = "hourly";
    serviceConfig = {
      Type = "oneshot";
      OnFailure = "status-email-root@%n.service";
    };
  };
  # ape – sync git mirrors
  systemd.services.ape = {
    description = "Ape - sync git mirrors";
    requires = [ "network-online.target" ];
    after    = [ "network-online.target" ];

    restartIfChanged = false;
    unitConfig.X-StopOnRemoval = false;

    serviceConfig = {
      Type = "oneshot";
      User = "vincent";
      OnFailure = "status-email-root@%n.service";
    };

    path = with pkgs; [ git ];
    script = ''
    ${pkgs.nur.repos.vdemeester.ape}/bin/ape up /home/vincent/var/mirrors
    '';

    startAt = "hourly";
  };
  # mr -i u daily
  systemd.services.mr = {
    description = "Update configs daily";
    requires = [ "network-online.target" ];
    after    = [ "network-online.target" ];

    restartIfChanged = false;
    unitConfig.X-StopOnRemoval = false;

    serviceConfig = {
      Type = "oneshot";
      User = "vincent";
      OnFailure = "status-email-root@%n.service";
    };

    path = with pkgs; [ git mr ];
    script = ''
    set -e
     cd /mnt/synodine/volumeUSB2/usbshare/src/github.com/vdemeester/configs/
     mr -t run git reset --hard
     mr -t u
    '';

    startAt = "daily";
  };
}
