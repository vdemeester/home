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
    kobe.A = [ (builtins.head globals.machines.kobe.net.ips) ];
    synodine.A = [ (builtins.head globals.machines.synodine.net.ips) ];

    # Hardcoded entries not in globals or incomplete in globals
    wakasu = {
      A = [ "192.168.1.77" ];
      subdomains."*".A = [ "192.168.1.77" ];
    };
    honshu.A = [ "192.168.1.17" ];
    remakrable.A = [ "192.168.1.57" ];
    hass.A = [ "192.168.1.181" ];

    # OpenShift infrastructure
    vm0.A = [ "192.168.1.120" ];
    vm1.A = [ "192.168.1.121" ];
    vm2.A = [ "192.168.1.122" ];
    vm3.A = [ "192.168.1.123" ];
    vm4.A = [ "192.168.1.124" ];
    vm5.A = [ "192.168.1.125" ];
    vm6.A = [ "192.168.1.126" ];
    vm7.A = [ "192.168.1.127" ];
    vm8.A = [ "192.168.1.128" ];
    vm9.A = [ "192.168.1.129" ];

    ocp = {
      subdomains = {
        api.A = [ "192.168.1.120" ];
        api-int.A = [ "192.168.1.120" ];
        apps.subdomains."*".A = [ "192.168.1.120" ];
        master0.A = [ "192.168.1.121" ];
        master1.A = [ "192.168.1.122" ];
        master3.A = [ "192.168.1.123" ];
        worker1.A = [ "192.168.1.124" ];
        worker2.A = [ "192.168.1.125" ];
        worker3.A = [ "192.168.1.126" ];
        worker4.A = [ "192.168.1.127" ];
        worker5.A = [ "192.168.1.128" ];
        bootstrap.A = [ "192.168.1.129" ];
        etcd-0.A = [ "192.168.1.121" ];
        etcd-1.A = [ "192.168.1.122" ];
        etcd-2.A = [ "192.168.1.123" ];
      };
      SRV = [
        {
          service = "etcd-server-ssl";
          proto = "tcp";
          priority = 0;
          weight = 10;
          port = 2380;
          target = "etcd-0.ocp.home.";
        }
        {
          service = "etcd-server-ssl";
          proto = "tcp";
          priority = 0;
          weight = 10;
          port = 2380;
          target = "etcd-1.ocp.home.";
        }
        {
          service = "etcd-server-ssl";
          proto = "tcp";
          priority = 0;
          weight = 10;
          port = 2380;
          target = "etcd-2.ocp.home.";
        }
      ];
    };

    # k8s nodes
    ubnt1.A = [ "192.168.1.130" ];
    ubnt2.A = [ "192.168.1.131" ];
    k8sn1.A = [ "192.168.1.130" ];
    k8sn2.A = [ "192.168.1.131" ];
    k8sn3.A = [ "192.168.1.132" ];
  }
  // mkHomeMachineRecords;
}
