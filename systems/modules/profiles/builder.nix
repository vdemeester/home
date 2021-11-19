{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption importTOML filter;
  cfg = config.profiles.externalbuilder;
  metadata = importTOML ../../../ops/hosts.toml;
  currentHostIP =
    if builtins.hasAttr "addrs" metadata.hosts.${config.networking.hostName}
    then metadata.hosts.${config.networking.hostName}.addrs.v4
    else "0.0.0.0";
  isCurrentHost = n: n.hostName != currentHostIP;
in
{
  options = {
    profiles.externalbuilder = {
      enable = mkEnableOption "Enable externalbuilder profile";
    };
  };
  config = mkIf cfg.enable {
    nix.distributedBuilds = true;
    sops.secrets.builder = {
      sopsFile = ../../../secrets/builder.yaml;
    };

    nix.buildMachines = (filter isCurrentHost
      [
        {
          hostName = "${metadata.hosts.wakasu.addrs.v4}";
          maxJobs = metadata.hosts.wakasu.builder.maxJobs;
          sshUser = "builder";
          sshKey = config.sops.secrets.builder.path;
          systems = metadata.hosts.wakasu.builder.systems;
          supportedFeatures = metadata.hosts.wakasu.builder.features;
        }
        {
          hostName = "${metadata.hosts.aomi.addrs.v4}";
          maxJobs = metadata.hosts.aomi.builder.maxJobs;
          sshUser = "builder";
          sshKey = config.sops.secrets.builder.path;
          systems = metadata.hosts.aomi.builder.systems;
          supportedFeatures = metadata.hosts.aomi.builder.features;
        }
      ]
    );

    programs.ssh.knownHosts = {
      "wakasu" = {
        hostNames = [ "wakasu.home" "${metadata.hosts.wakasu.addrs.v4}" ];
        publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ2GB030S1+iZMqwgYhkl5CuBOKBjZoujc0aVHII39/x";
      };
      "aomi" = {
        hostNames = [ "aomi.home" "${metadata.hosts.aomi.addrs.v4}" ];
        publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFQVlSrUKU0xlM9E+sJ8qgdgqCW6ePctEBD2Yf+OnyME";
      };
    };

  };


}
