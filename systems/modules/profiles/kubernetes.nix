{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkOption mkIf mkDefault mkOverride types optionals;
  cfg = config.profiles.kubernetes;
in
{
  options.profiles.kubernetes = {
    master = {
      enable = mkEnableOption "Make this node a master node";
      ip = mkOption {
        description = "master node address";
        type = types.str;
      };
      hostname = mkOption {
        default = "api.kube";
        description = "master node hostname";
        type = types.str;
      };
      port = mkOption {
        default = 6443;
        description = "port the apiserver will expose";
        type = types.int;
      };
    };
  };
  config = mkIf cfg.enable {
    networking = {
      firewall.allowedTCPPorts = [ 80 443 6443 ];
      extraHosts = "${cfg.master.ip} ${cfg.master.hostname}";
    };

    boot.kernelModules = [ "ceph" ];

    # packages for administration tasks
    environment.systemPackages = with pkgs; [
      kubectl
      kubernetes
    ];

    services.kubernetes = {
      roles = [ "node" ] ++ optionals cfg.master.enable [ "master" ];
      masterAddress = cfg.master.hostname;
      apiserverAddress = "https://${cfg.master.hostname}:${toString cfg.master.port}";
      kubeconfig.server = "https://${cfg.master.hostname}:${toString cfg.master.port}";
      easyCerts = true;
      apiserver = mkIf cfg.master.enable {
        securePort = cfg.master.port;
        advertiseAddress = cfg.master.ip;
      };
      controllerManager.extraOpts = "--horizontal-pod-autoscaler-use-rest-clients=false";
      # use coredns
      addons.dns.enable = true;

      # needed if you use swap
      kubelet.extraOpts = "--fail-swap-on=false --root-dir=/var/lib/kubelet";
    };
  };
}
