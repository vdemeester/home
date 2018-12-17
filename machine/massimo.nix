{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  time.timeZone = "Europe/Paris";
  fileSystems."/mnt/synodine" = {
    device = "192.168.12.19:/";
    fsType = "nfs";
    options = ["x-systemd.automount" "noauto"];
  };
  boot = {
    cleanTmpDir = true;
  };
  profiles = {
    avahi.enable = true;
    git.enable = true;
    ssh.enable = true;
    syncthing.enable = true;
  };
  networking.firewall.allowPing = true;
  networking.firewall.allowedTCPPorts = [ 5000 ];
  services = {
    nix-binary-cache = {
      enable = true;
      domain = "massimo.local";
      aliases = ["cache.massimo.home" "nix.cache.home"];
    };
    athens = {
      enable = true;
      user = "vincent";
    };
    dockerRegistry = {
      enable = true;
      enableGarbageCollect = true;
      listenAddress = "0.0.0.0";
    };
    logind.extraConfig = "HandleLidSwitch=ignore";
    syncthing-edge.guiAddress = "${wireguard.ips.massimo}:8384";
    wireguard = {
      enable = true;
      ips = [ "${wireguard.ips.massimo}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };
  };
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGR4dqXwHwPpYgyk6yl9+9LRL3qrBZp3ZWdyKaTiXp0p vincent@shikoku"
  ];
}
