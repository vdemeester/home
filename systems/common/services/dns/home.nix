{ dns, globals, ... }:
with dns.lib.combinators;
let
  # Machines with home network IPs that should have wildcards
  machinesWithWildcard = [
    "okinawa"
    "sakhalin"
    "aomi"
    "rhea"
    "aion"
    "shikoku"
    "athena"
    "demeter"
    "nagoya"
  ];

  mkHomeMachineRecords = builtins.listToAttrs (
    map (machineName: {
      name = machineName;
      value =
        let
          homeIP = globals.machines.${machineName}.net.ips;
          ip = if builtins.isList homeIP then builtins.head homeIP else homeIP;
        in
        {
          A = [ ip ];
          subdomains."*".A = [ ip ];
        };
    }) machinesWithWildcard
  );
in
{
  SOA = {
    nameServer = "ns1.home.";
    adminEmail = "admin.home";
    serial = 3;
    refresh = 604800;
    retry = 86400;
    expire = 2419200;
    minimum = 604800;
  };

  NS = [
    "ns1.home."
    "ns2.home."
  ];

  subdomains = {
    # Name servers
    ns1.A = [ (builtins.head globals.machines.demeter.net.ips) ];
    ns2.A = [ (builtins.head globals.machines.athena.net.ips) ];

    # Cache wildcard
    cache.subdomains."*".A = [ (builtins.head globals.machines.sakhalin.net.ips) ];

    # Machines without wildcards
    hokkaido.A = [ (builtins.head globals.machines.hokkaido.net.ips) ];
    synodine.A = [ (builtins.head globals.machines.synodine.net.ips) ];

    # Hardcoded entries not in globals or incomplete in globals
    wakasu = {
      A = [ "192.168.1.77" ];
      subdomains."*".A = [ "192.168.1.77" ];
    };
    honshu.A = [ "192.168.1.17" ];
    remakrable.A = [ "192.168.1.57" ];
    hass.A = [ "192.168.1.181" ];

    # OpenShift SNO lab
    lab = {
      subdomains = {
        # OCP4 SNO cluster
        ocp4 = {
          subdomains = {
            # Point to aomi (host) which will forward to the VM
            api.A = [ "192.168.1.23" ];
            api-int.A = [ "192.168.1.23" ];
            apps.subdomains."*".A = [ "192.168.1.23" ];
            # Direct VM access for internal use
            master-sno.A = [ "192.168.100.7" ];
          };
        };
      };
    };
  }
  // mkHomeMachineRecords;
}
