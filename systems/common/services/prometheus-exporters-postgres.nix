_: {
  services.prometheus.exporters.postgres = {
    enable = true;
    port = 9187;
    runAsLocalSuperUser = true;
  };

  networking.firewall.allowedTCPPorts = [ 9187 ];
}
