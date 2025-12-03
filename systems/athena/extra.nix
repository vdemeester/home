{
  pkgs,
  libx,
  globals,
  ...
}:
{
  imports = [
    ../common/services/bind.nix
    ../common/services/prometheus-exporters-node.nix
    ../common/services/prometheus-exporters-bind.nix
  ];

  networking.firewall.enable = false;

  # TODO make it an option ? (otherwise I'll add it for all)
  users.users.vincent.linger = true;

  systemd.services.nginx.serviceConfig.ReadWritePaths = [ "/var/www/" ];
  services = {
    # Dual-hub client: connect to both local hub (demeter) and remote hub (kerkouane)
    wireguard.dualClient = {
      enable = true;
      ips = libx.wg-ips globals.machines.athena.net.vpn.ips;

      # Local hub (demeter)
      localHub = {
        enable = true;
        endpoint = builtins.head globals.machines.${globals.net.localHub.host}.net.ips;
        endpointPort = globals.net.localHub.port;
        endpointPublicKey = globals.machines.${globals.net.localHub.host}.net.vpn.pubkey;
      };

      # Remote hub (kerkouane)
      remoteHub = {
        endpoint = globals.net.vpn.endpoint;
        endpointPort = globals.net.vpn.port;
        endpointPublicKey = globals.machines.kerkouane.net.vpn.pubkey;
      };
    };
    nginx = {
      enable = true;
      statusPage = true;
      package = pkgs.nginxMainline.override (_old: {
        modules = with pkgs.nginxModules; [
          fancyindex
          dav
        ];
      });
      recommendedGzipSettings = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      virtualHosts."dav.athena.sbr.pm" = {
        locations."/" = {
          root = "/var/www/dav";
          basicAuthFile = "/var/www/dav.auth";
          extraConfig = ''
            						autoindex on;
                        set $x $uri$request_method;
                        if ($x ~ [^/]MKCOL$) {
                            rewrite ^(.*)$ $1/;
                        }
                        dav_methods PUT DELETE MKCOL COPY MOVE;
                        dav_ext_methods PROPFIND OPTIONS;
                        dav_access user:rw group:rw all:r;
                        client_body_temp_path /var/cache/nginx;
                        create_full_put_path on;
                        # add_header 'Access-Control-Allow-Origin' '$ALLOWED_ORIGIN' always;
                        # add_header 'Access-Control-Allow-Methods' 'GET, POST, PUT, DELETE, MKCOL, COPY, MOVE, PROPFIND, OPTIONS' always;
                        # add_header 'Access-Control-Allow-Headers' 'Origin,Accept,X-Requested-With,Content-Type,Access-Control-Request-Method,Access-Control-Request-Headers,Authorization,X-CSRF-Token,Depth' always;
                        # add_header 'Access-Control-Allow-Credentials' 'true' always;
          '';
        };
      };
    };
  };
}
