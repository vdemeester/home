{ dns, globals, ... }:
with dns.lib.combinators;
let
  dnsHelpers = import ../../../../lib/dns-helpers.nix { inherit globals; };
  inherit (dnsHelpers) getMachineIP mkServiceRecords;

  # Only include machines that should be in sbr.pm zone
  machineList = [
    "shikoku"
    "sakhalin"
    "aix"
    "rhea"
    "aion"
    "demeter"
    "athena"
    "nagoya"
    "kerkouane"
    "aomi"
    "kyushu"
    "wakasu"
  ];

  mkMachineRecords = builtins.listToAttrs (
    map (machineName: {
      name = machineName;
      value = {
        A = [ (getMachineIP globals.machines.${machineName}) ];
        subdomains."*".A = [ (getMachineIP globals.machines.${machineName}) ];
      };
    }) machineList
  );
in
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

  # Root domain points to public endpoint
  A = [ "167.99.17.238" ];

  # Email (Gandi)
  MX = [
    {
      preference = 10;
      exchange = "spool.mail.gandi.net.";
    }
    {
      preference = 50;
      exchange = "fb.mail.gandi.net.";
    }
  ];

  subdomains = {
    # Name servers (demeter and athena)
    ns1.A = [ (getMachineIP globals.machines.demeter) ];
    ns2.A = [ (getMachineIP globals.machines.athena) ];

    # Wildcard for public endpoint
    "*".A = [
      {
        address = "167.99.17.238";
        ttl = 10800;
      }
    ];

    # Email CNAMEs (Gandi mail service)
    imap.CNAME = [ "access.mail.gandi.net." ];
    pop.CNAME = [ "access.mail.gandi.net." ];
    smtp.CNAME = [ "relay.mail.gandi.net." ];
    webmail.CNAME = [ "webmail.gandi.net." ];

    # Shortcuts
    p.A = [ "167.99.17.238" ]; # public endpoint shortcut
    www = {
      A = [ "167.99.17.238" ];
      subdomains."*".A = [ "167.99.17.238" ];
    };
  }
  // mkMachineRecords
  // mkServiceRecords globals.services;
}
