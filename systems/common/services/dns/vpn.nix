{ dns, ... }:
with dns.lib.combinators;
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
    ns1.A = [ "10.100.0.2" ];
    ns2.A = [ "10.100.0.16" ];

    # Cache/Massimo wildcards
    cache.subdomains."*".A = [ "10.100.0.6" ];
    massimo.subdomains."*".A = [ "10.100.0.6" ];

    # Machines with wildcards
    okinawa = {
      A = [ "10.100.0.14" ];
      subdomains."*".A = [ "10.100.0.14" ];
    };
    aomi = {
      A = [ "10.100.0.17" ];
      subdomains."*".A = [ "10.100.0.17" ];
    };
    shikoku = {
      A = [ "10.100.0.2" ];
      subdomains."*".A = [ "10.100.0.2" ];
    };
    sakhalin = {
      A = [ "10.100.0.16" ];
      subdomains."*".A = [ "10.100.0.16" ];
    };
    rhea = {
      A = [ "10.100.0.50" ];
      subdomains."*".A = [ "10.100.0.50" ];
    };
    aion = {
      A = [ "10.100.0.49" ];
      subdomains."*".A = [ "10.100.0.49" ];
    };
    athena = {
      A = [ "10.100.0.83" ];
      subdomains."*".A = [ "10.100.0.83" ];
    };
    demeter = {
      A = [ "10.100.0.82" ];
      subdomains."*".A = [ "10.100.0.82" ];
    };
    nagoya = {
      A = [ "10.100.0.80" ];
      subdomains."*".A = [ "10.100.0.80" ];
    };
    kyushu = {
      A = [ "10.100.0.19" ];
      subdomains."*".A = [ "10.100.0.19" ];
    };
    hass.A = [ "10.100.0.81" ];
  };
}
