{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ./home.nix ];
  boot = {
    cleanTmpDir = true;
  };
  networking = {
    firewall = {
      allowPing = true;
      allowedTCPPorts = [ 5000 ];
    };
  };
  profiles = {
    avahi.enable = true;
    git.enable = true;
    nix-config.buildCores = 4;
    ssh.enable = true;
    syncthing.enable = true;
  };
  services = {
    nix-binary-cache = {
      enable = true;
      domain = "massimo.local";
      aliases = ["cache.massimo.home" "nix.cache.home"];
    };
    coredns = {
      enable = true;
      names = dns;
    };
    athens = {
      enable = true;
      user = "vincent";
    };
    dockerRegistry = {
      enable = true;
      enableGarbageCollect = true;
      listenAddress = "0.0.0.0";
      extraConfig = {
        proxy = {
          remoteurl = "https://registry-1.docker.io";
        };
      };
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
}
