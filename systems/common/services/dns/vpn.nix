{ dns, globals, ... }:
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
          vpnIP = globals.machines.${machineName}.net.vpn.ips;
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
    ns1.A = [ (builtins.head globals.machines.shikoku.net.vpn.ips) ];
    ns2.A = [ (builtins.head globals.machines.sakhalin.net.vpn.ips) ];

    # Cache/Massimo wildcards - these don't exist in globals, keeping hardcoded
    cache.subdomains."*".A = [ "10.100.0.6" ];
    massimo.subdomains."*".A = [ "10.100.0.6" ];

    # hass - hardcoded as it's not in the machine list
    hass.A = [ "10.100.0.81" ];
  }
  // mkVpnMachineRecords;
}
