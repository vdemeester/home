{ dns, ... }:
with dns.lib.combinators;
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
    # Workstations and devices
    "11".PTR = [ "hokkaido.home." ];
    "17".PTR = [ "honshu.home." ];
    "18".PTR = [ "kobe.home." ];
    "19".PTR = [ "okinawa.home." ];
    "70".PTR = [ "sakhalin.home." ];
    "20".PTR = [ "synodine.home." ];
    "77".PTR = [ "wakasu.home." ];
    "23".PTR = [ "aomi.home." ];
    "50".PTR = [ "rhea.home." ];
    "49".PTR = [ "aion.home." ];
    "24".PTR = [ "shikoku.home." ];
    "57".PTR = [ "remarkable.home." ];
    "15".PTR = [ "honshu.home." ];
    "181".PTR = [ "hass.home." ];

    # Servers (primary PTR)
    "182".PTR = [ "demeter.home." ];
    "183".PTR = [ "athena.home." ];

    # OpenShift VMs - Load Balancer (primary PTR)
    "120".PTR = [ "vm0.home." ];

    # Masters (primary PTR)
    "121".PTR = [ "vm1.home." ];
    "122".PTR = [ "vm2.home." ];
    "123".PTR = [ "vm3.home." ];

    # Workers (primary PTR)
    "124".PTR = [ "vm4.home." ];
    "125".PTR = [ "vm5.home." ];
    "126".PTR = [ "vm6.home." ];
    "127".PTR = [ "vm7.home." ];
    "128".PTR = [ "vm8.home." ];

    # Bootstrap (primary PTR)
    "129".PTR = [ "vm9.home." ];

    # k8s nodes (primary PTR)
    "130".PTR = [ "ubnt1.home." ];
    "131".PTR = [ "ubnt2.home." ];
    "132".PTR = [ "k8sn3.home." ];
  };
}
