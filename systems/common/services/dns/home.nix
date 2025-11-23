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
    # Name servers
    ns1.A = [ "192.168.1.182" ];
    ns2.A = [ "192.168.1.183" ];

    # Cache wildcard
    cache.subdomains."*".A = [ "192.168.1.70" ];

    # Machines with wildcards
    okinawa = {
      A = [ "192.168.1.19" ];
      subdomains."*".A = [ "192.168.1.19" ];
    };
    hokkaido.A = [ "192.168.1.11" ];
    honshu.A = [ "192.168.1.17" ];
    kobe.A = [ "192.168.1.18" ];
    sakhalin = {
      A = [ "192.168.1.70" ];
      subdomains."*".A = [ "192.168.1.70" ];
    };
    synodine.A = [ "192.168.1.20" ];
    wakasu = {
      A = [ "192.168.1.77" ];
      subdomains."*".A = [ "192.168.1.77" ];
    };
    aomi = {
      A = [ "192.168.1.23" ];
      subdomains."*".A = [ "192.168.1.23" ];
    };
    rhea = {
      A = [ "192.168.1.50" ];
      subdomains."*".A = [ "192.168.1.50" ];
    };
    aion = {
      A = [ "192.168.1.49" ];
      subdomains."*".A = [ "192.168.1.49" ];
    };
    shikoku = {
      A = [ "192.168.1.24" ];
      subdomains."*".A = [ "192.168.1.24" ];
    };
    athena = {
      A = [ "192.168.1.183" ];
      subdomains."*".A = [ "192.168.1.183" ];
    };
    demeter = {
      A = [ "192.168.1.182" ];
      subdomains."*".A = [ "192.168.1.182" ];
    };
    nagoya = {
      A = [ "192.168.1.80" ];
      subdomains."*".A = [ "192.168.1.80" ];
    };
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
  };
}
