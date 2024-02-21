{ pkgs, lib, ... }:

with lib;
let
  hostname = "athena";
  # secretPath = ../../secrets/machines.nix;
  # secretCondition = (builtins.pathExists secretPath);
  # 
  # ip = strings.optionalString secretCondition (import secretPath).wireguard.ips."${hostname}";
  # ips = lists.optionals secretCondition ([ "${ip}/24" ]);
  # endpointIP = strings.optionalString secretCondition (import secretPath).wg.endpointIP;
  # endpointPort = if secretCondition then (import secretPath).wg.listenPort else 0;
  # endpointPublicKey = strings.optionalString secretCondition (import secretPath).wireguard.kerkouane.publicKey;

  metadata = importTOML ../../ops/hosts.toml;
in
{
  imports = [
    # (import ../../users/vincent)
    # (import ../../users/root)
  ];

  networking = {
    hostName = hostname;
    firewall.enable = false; # we are in safe territory :D
    # bridges.br1.interfaces = [ "enp0s31f6" ];
    # useDHCP = false;
    # interfaces.br1 = {
    #   useDHCP = true;
    # };
  };

  # core.boot.systemd-boot = lib.mkForce true;
  # profiles.base.systemd-boot = lib.mkForce true;
  # 
  # modules = {
  #   services = {
  #     syncthing = {
  #       enable = true;
  #       guiAddress = "${metadata.hosts.sakhalin.wireguard.addrs.v4}:8384";
  #     };
  #     avahi.enable = true;
  #     ssh.enable = true;
  #   };
  # };
  # 
  # profiles = {
  #   bind.enable = true;
  #   home = true;
  # };

  # services = {
  #   wireguard = {
  #     enable = true;
  #     ips = ips;
  #     endpoint = endpointIP;
  #     endpointPort = endpointPort;
  #     endpointPublicKey = endpointPublicKey;
  #   };
  # };
  security.apparmor.enable = true;
  security.pam.enableSSHAgentAuth = true;
}
