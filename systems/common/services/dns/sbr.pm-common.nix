# Common DNS zone builder for sbr.pm
# Takes an IP selector function to allow different IP selection strategies
{
  dns,
  globals,
  getIPForMachine,
}:
with dns.lib.combinators;
let
  # Helper to generate service DNS records
  mkServiceRecords =
    services:
    builtins.listToAttrs (
      builtins.concatMap (
        serviceName:
        let
          service = services.${serviceName};
          hostName = if builtins.isAttrs service then service.host else service;
          ip = getIPForMachine globals.machines.${hostName};
          aliases = if builtins.isAttrs service then (service.aliases or [ ]) else [ ];
        in
        [
          {
            name = serviceName;
            value.A = [ ip ];
          }
        ]
        ++ (map (alias: {
          name = alias;
          value.A = [ ip ];
        }) aliases)
      ) (builtins.attrNames services)
    );

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
        A = [ (getIPForMachine globals.machines.${machineName}) ];
        subdomains."*".A = [ (getIPForMachine globals.machines.${machineName}) ];
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
    ns1.A = [ (getIPForMachine globals.machines.demeter) ];
    ns2.A = [ (getIPForMachine globals.machines.athena) ];

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
