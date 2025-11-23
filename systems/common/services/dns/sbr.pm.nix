{ dns, ... }:
with dns.lib.combinators;
{
  SOA = {
    nameServer = "ns1.sbr.pm.";
    adminEmail = "admin.sbr.pm";
    serial = 3;
    refresh = 604800;
    retry = 86400;
    expire = 2419200;
    minimum = 604800;
  };

  NS = [
    "ns1.sbr.pm."
    "ns2.sbr.pm."
  ];

  subdomains = {
    # Name servers
    ns1.A = [ "192.168.1.182" ];
    ns2.A = [ "192.168.1.183" ];

    # Wildcard for public endpoint
    "*".A = [
      {
        address = "167.99.17.238";
        ttl = 10800;
      }
    ];

    # Machines
    wakasu = {
      A = [ "192.168.1.77" ];
      subdomains."*".A = [ "192.168.1.77" ];
    };
    shikoku = {
      A = [ "192.168.1.24" ];
      subdomains."*".A = [ "192.168.1.24" ];
    };
    sakhalin = {
      A = [ "192.168.1.70" ];
      subdomains."*".A = [ "192.168.1.70" ];
    };
    aix = {
      A = [ "10.100.0.89" ];
      subdomains."*".A = [ "10.100.0.89" ];
    };
    rhea = {
      A = [ "192.168.1.50" ];
      subdomains."*".A = [ "192.168.1.50" ];
    };
    aion = {
      A = [ "192.168.1.49" ];
      subdomains."*".A = [ "192.168.1.49" ];
    };
    demeter = {
      A = [ "192.168.1.182" ];
      subdomains."*".A = [ "192.168.1.182" ];
    };
    athena = {
      A = [ "192.168.1.183" ];
      subdomains."*".A = [ "192.168.1.183" ];
    };
    honshu = {
      A = [ "192.168.1.15" ];
      subdomains."*".A = [ "192.168.1.15" ];
    };
    nagoya = {
      A = [ "192.168.1.80" ];
      subdomains."*".A = [ "192.168.1.80" ];
    };
    kerkouane = {
      A = [ "10.100.0.1" ];
      subdomains."*".A = [ "10.100.0.1" ];
    };

    # Rhea media services
    jellyfin.A = [ "192.168.1.50" ];
    jellyseerr.A = [ "192.168.1.50" ];
    sonarr.A = [ "192.168.1.50" ];
    radarr.A = [ "192.168.1.50" ];
    lidarr.A = [ "192.168.1.50" ];
    bazarr.A = [ "192.168.1.50" ];
    transmission.A = [ "192.168.1.50" ];
    t.A = [ "192.168.1.50" ];
  };
}
