_: {
  services.acpid = {
    enable = true;
  };
  systemd.services.acpid.serviceConfig = {
    ProtectSystem = "full";
    ProtectHome = true;
    RestrictAddressFamilies = [
      "AF_INET"
      "AF_INET6"
    ];
    SystemCallFilter = "~@clock @cpu-emulation @debug @module @mount @raw-io @reboot @swap";
    ProtectKernelTunables = true;
    ProtectKernelModules = true;
  };
}
