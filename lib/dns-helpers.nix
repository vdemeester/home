{ globals }:
{
  # Helper to get first IP from machine config
  # Prefers regular IPs, fallback to VPN IPs
  getMachineIP =
    machine:
    let
      ips = machine.net.ips or [ ];
      vpnIps = machine.net.vpn.ips or [ ];
      # Prefer regular IPs, fallback to VPN IPs
      allIps = if ips != [ ] then ips else vpnIps;
    in
    if builtins.isList allIps then builtins.head allIps else allIps;

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
  mkServiceRecords =
    services:
    builtins.listToAttrs (
      builtins.concatMap (
        serviceName:
        let
          service = services.${serviceName};
          hostName = if builtins.isAttrs service then service.host else service;
          hostIP = globals.machines.${hostName}.net.ips;
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
