{ lib, config, ... }:
{
  options = {
    prometheus-exporters-node.collectors = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "systemd"
        "processes"
      ];
    };
    prometheus-exporters-node.extraFlags = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "--collector.ethtool"
        "--collector.softirqs"
        "--collector.tcpstat"
      ];
    };
  };

  config = {
    services.prometheus.exporters.node = {
      enable = true;
      port = 9000;
      enabledCollectors = config.prometheus-exporters-node.collectors;
      inherit (config.prometheus-exporters-node) extraFlags;
    };
  };
}
