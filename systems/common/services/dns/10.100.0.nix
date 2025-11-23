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
    # Name servers (note: shikoku is both ns1 and a regular host)
    "2".PTR = [ "shikoku.vpn." ];
    "14".PTR = [ "okinawa.vpn." ];
    "8".PTR = [ "wakasu.vpn." ];
    "17".PTR = [ "aomi.vpn." ];
    "16".PTR = [ "sakhalin.vpn." ];
    "50".PTR = [ "rhea.vpn." ];
    "49".PTR = [ "aion.vpn." ];
    "83".PTR = [ "athena.vpn." ];
    "82".PTR = [ "demeter.vpn." ];
    "81".PTR = [ "hass.vpn." ];
  };
}
