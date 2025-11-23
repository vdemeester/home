{ globals }:
{
  # Helper to get first IP from machine config
  # Uses VPN IPs only (10.100.0.x) for public DNS
  getMachineIP =
    machine:
    let
      vpnIps = machine.net.vpn.ips or [ ];
    in
    if builtins.isList vpnIps then builtins.head vpnIps else vpnIps;

  # Generate machine subdomains with wildcard support
  # Takes a list of machine names and returns an attribute set of DNS records
  mkMachineRecords =
    machineList:
    builtins.listToAttrs (
      map (machineName: {
        name = machineName;
        value = {
          A = [ (globals.machines.${machineName}.net.ips or (globals.machines.${machineName}.net.vpn.ips)) ];
          subdomains."*".A = [
            (globals.machines.${machineName}.net.ips or (globals.machines.${machineName}.net.vpn.ips))
          ];
        };
      }) machineList
    );

  # Helper to generate service DNS records from globals
  # Takes a services attribute set and returns DNS records with alias support
  # Uses VPN IPs only (10.100.0.x) for public DNS
  mkServiceRecords =
    services:
    builtins.listToAttrs (
      builtins.concatMap (
        serviceName:
        let
          service = services.${serviceName};
          hostName = if builtins.isAttrs service then service.host else service;
          hostIP = globals.machines.${hostName}.net.vpn.ips;
          ip = if builtins.isList hostIP then builtins.head hostIP else hostIP;
          aliases = if builtins.isAttrs service then (service.aliases or [ ]) else [ ];
        in
        [
          {
            name = serviceName;
            value.A = [ ip ];
          }
        ]
        ++ (map (alias: {
          name = alias;
          value.A = [ ip ];
        }) aliases)
      ) (builtins.attrNames services)
    );
}
