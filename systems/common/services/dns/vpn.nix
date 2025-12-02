{ dns, config, ... }:
with dns.lib.combinators;
let
  # Machines that have VPN entries
  machineList = [
    "okinawa"
    "aomi"
    "shikoku"
    "sakhalin"
    "rhea"
    "aion"
    "athena"
    "demeter"
    "nagoya"
    "kyushu"
  ];

  mkVpnMachineRecords = builtins.listToAttrs (
    map (machineName: {
      name = machineName;
      value =
        let
          vpnIP = config.infrastructure.machines.${machineName}.net.vpn.ips;
          ip = if builtins.isList vpnIP then builtins.head vpnIP else vpnIP;
        in
        {
          A = [ ip ];
          subdomains."*".A = [ ip ];
        };
    }) machineList
  );
in
{
  SOA = {
    nameServer = "ns1.vpn.";
    adminEmail = "admin.vpn";
    serial = 3;
    refresh = 604800;
    retry = 86400;
    expire = 2419200;
    minimum = 604800;
  };

  NS = [
    "ns1.vpn."
    "ns2.vpn."
  ];

  subdomains = {
    # Name servers
    ns1.A = [ (builtins.head config.infrastructure.machines.shikoku.net.vpn.ips) ];
    ns2.A = [ (builtins.head config.infrastructure.machines.sakhalin.net.vpn.ips) ];

    # hass - hardcoded as it's not in the machine list
    hass.A = [ "10.100.0.81" ];
  }
  // mkVpnMachineRecords;
}
