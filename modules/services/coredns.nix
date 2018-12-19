{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.coredns;
  names = builtins.attrNames cfg.names;

  invert-suffix = ip:
    let
      elts = builtins.split "[\.]" ip;
    in "${builtins.elemAt elts 6}.${builtins.elemAt elts 4}";

  toNSFile = mapAttrs' (name: value: nameValuePair ("coredns/db.${name}") { text = toNS name value.entries; });
  toReverseNSFile = mapAttrs' (name: value: nameValuePair ("coredns/db.${value.reverse}") { text = toReverseNS name value.entries; });
  toCorefile = names: {"coredns/Corefile"= {text = ''
.:53 {

  ${concatStrings (kToFile names)}

  proxy . 8.8.8.8
  log stdout
  errors
}
  '';
  };
  };

  kToNS = nsName: mapAttrsToList (name: value:
      "${name}.${nsName}. IN A ${value}\n"
  );
  kToReverseNS = nsName: mapAttrsToList(name: value:
    if hasPrefix "*" name
    then
     "" # no "reverse" for wildcard domain
    else
    "${invert-suffix value} IN PTR ${name}.${nsName}.\n"
  );

  kToFile = names: mapAttrsToList (n: v: "${v}") (mapAttrs (name: value: ''
  file /etc/coredns/db.${name} ${name}
  file /etc/coredns/db.${value.reverse} ${value.reverse}.in-addr.arpa
'') names);

  toNS = (name: ns: ''
$TTL    604800
@    IN    SOA    ns1.${name}. admin.${name}. (
                  3        ; Serial
             604800        ; Refresh
              86400        ; Retry
            2419200        ; Expire
             604800 )    ; Negative Cache TTL

; name servers - NS records
@ IN NS ns1

${concatStrings (kToNS name ns)}
  '');
  toReverseNS = (name: ns: ''
$TTL    604800
@    IN    SOA    ns1.${name}. admin.${name}. (
                  3        ; Serial
             604800        ; Refresh
              86400        ; Retry
            2419200        ; Expire
             604800 )    ; Negative Cache TTL

; name servers - NS records
@ IN NS ns1.${name}.

${concatStrings (kToReverseNS name ns)}
'');
in
{
  options = {
    services.coredns = {
      enable = mkEnableOption ''
      CoreDNS is a DNS server implemented in Go
      '';
      package = mkOption {
        type = types.package;
        default = pkgs.coredns;
        description = ''
          CoreDNS package to use.
        '';
      };
      names = mkOption {
        type = types.attrs;
        default = {};
        example = { "foo" = { reverse = "1.1"; entries = { "bar" = "1.1.1.1"; }; }; };
        description = ''
          Names to setup in coredns
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    networking.firewall = {
      allowedUDPPorts = [ 53 ];
      allowedTCPPorts = [ 53 ];
    };
    systemd.packages = [ cfg.package ];

    # NEW
    environment.etc = toNSFile cfg.names
    // toReverseNSFile cfg.names
    // toCorefile cfg.names;

    systemd.services.coredns = {
      description = "CoreDNS service";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Restart = "on-failure";
        ExecStart = ''
          ${cfg.package}/bin/coredns -conf /etc/coredns/Corefile
        '';
      };
      path = [ cfg.package ];
    };
  };
}
