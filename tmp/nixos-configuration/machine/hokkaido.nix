{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ../hardware/thinkpad-x220.nix ./home.nix ];
  boot = {
    kernel.sysctl = {
      "net.bridge.bridge-nf-call-arptables" = 0;
      "net.bridge.bridge-nf-call-iptables" = 0;
      "net.bridge.bridge-nf-call-ip6tables" = 0;
    };
  };
  profiles = {
    avahi.enable = true;
    dev.enable = true;
    ssh.enable = true;
    syncthing.enable = true;
    nix-config.buildCores = 2;
    virtualization = {
      enable = true;
      nested = true;
      listenTCP = true;
    };
  };
  services = {
    logind = {
      lidSwitch = "ignore";
    };
    syncthing.guiAddress = "0.0.0.0:8384";
    wireguard = {
      enable = true;
      ips = [ "${wireguard.ips.hokkaido}/24" ];
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
}
