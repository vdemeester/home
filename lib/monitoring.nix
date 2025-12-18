{ lib }:
{
  # Filter machines with node exporters
  # Excludes non-NixOS machines, mobile devices, and machines without network config
  machinesWithNodeExporter =
    machines:
    lib.filterAttrs (
      name: machine:
      # Has network configuration with names (indicates it's a real host)
      (machine ? net && machine.net ? names)
      # Exclude mobile devices and tablets
      && !(builtins.elem name [
        "hokkaido"
        "suzu"
        "osaka"
      ])
      # Exclude non-NixOS machines that don't run node exporter
      && !(builtins.elem name [
        "synodine"
        "hass"
        "wakasu"
        "okinawa"
        "kobe"
      ])
    ) machines;

  # Generate Prometheus targets from machine list
  # Returns list of "hostname.domain:port" strings
  mkPrometheusTargets =
    {
      machines,
      domain ? "sbr.pm",
      port,
    }:
    lib.mapAttrsToList (name: _machine: "${name}.${domain}:${toString port}") machines;
}
