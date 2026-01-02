{
  globals,
  lib,
  libx,
  ...
}:
let
  # Common security headers for Caddy
  securityHeaders = ''
    header {
      Strict-Transport-Security "max-age=31536000; includeSubDomains"
      X-Content-Type-Options "nosniff"
      X-Frame-Options "SAMEORIGIN"
      Content-Security-Policy "default-src 'self' *.sbr.pm *.demeester.fr"
      X-XSS-Protection "1; mode=block"
      Cache-Control "public, max-age=604800, immutable"
    }
  '';
in
{
  imports = [
    ../common/services/prometheus-exporters-node.nix
    # ../common/services/syncthing.nix
  ];

  # TODO make it an option ? (otherwise I'll add it for all)
  users.users.vincent.linger = true;

  # Disable TPM2 (VPS has no TPM hardware)
  security.tpm2.enable = lib.mkForce false;

  services.openssh = {
    listenAddresses = [
      {
        addr = builtins.head globals.machines.kerkouane.net.vpn.ips;
        port = 22;
      }
    ];
    openFirewall = lib.mkForce false;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "without-password";
    };
  };

  services.wireguard.server = {
    enable = true;
    ips = libx.wg-ips globals.machines.kerkouane.net.vpn.ips;
    peers = libx.generateWireguardPeers globals.machines;
  };

  services.gosmee = {
    enable = true;
    public-url = "https://webhook.sbr.pm";
  };

  services.ntfy-sh = {
    enable = true;
    settings = {
      base-url = "https://ntfy.sbr.pm";
      upstream-base-url = "https://ntfy.sh";
      listen-http = "localhost:8111";
      behind-proxy = true;
      enable-login = true;
      auth-default-access = "deny-all";
    };
  };

  # Should probably move to hardware.nix
  networking.firewall.allowPing = true;
  networking.firewall.allowedTCPPorts = [
    80
    443
  ];
  services.caddy = {
    enable = true;
    email = "vincent@sbr.pm";

    # Enable Prometheus metrics on VPN interface only
    globalConfig = ''
      admin ${builtins.head globals.machines.kerkouane.net.vpn.ips}:2019

      servers {
        metrics
      }
    '';

    virtualHosts = {
      # File server with directory browsing (replaces fancyindex)
      "dl.sbr.pm".extraConfig = ''
        root * /var/www/dl.sbr.pm
        file_server browse {
          hide .fancyindex README.md HEADER.md
        }

        ${securityHeaders}
      '';

      # Alias for dl.sbr.pm
      "files.sbr.pm".extraConfig = ''
        redir https://dl.sbr.pm{uri} permanent
      '';

      # ntfy - reverse proxy with websockets
      "ntfy.sbr.pm".extraConfig = ''
        reverse_proxy localhost:8111
      '';

      # Static sites
      "paste.sbr.pm".extraConfig = ''
        root * /var/www/paste.sbr.pm
        file_server
        ${securityHeaders}
      '';

      "sbr.pm".extraConfig = ''
        root * /var/www/sbr.pm
        file_server
        ${securityHeaders}
      '';

      # Go vanity URL service
      "go.sbr.pm".extraConfig = ''
        reverse_proxy localhost:8080
        ${securityHeaders}
      '';

      # Whoami service (remote)
      "whoami.sbr.pm".extraConfig = ''
        reverse_proxy 10.100.0.8:80 {
          header_up Host {host}
          header_up X-Forwarded-For {remote_host}
        }
      '';

      # Immich photo management (proxied to rhea)
      "immich.sbr.pm".extraConfig = ''
        reverse_proxy 10.100.0.50:2283 {
          header_up Host {host}
          header_up X-Forwarded-For {remote_host}
          header_up X-Real-IP {remote_host}
          header_up X-Forwarded-Proto {scheme}
        }
      '';

      # Navidrome music streaming (proxied to aion)
      "navidrome.sbr.pm".extraConfig = ''
        reverse_proxy 10.100.0.49:4533 {
          header_up Host {host}
          header_up X-Forwarded-For {remote_host}
          header_up X-Real-IP {remote_host}
          header_up X-Forwarded-Proto {scheme}
        }
      '';

      # Jellyfin media server (proxied to rhea)
      "jellyfin.sbr.pm".extraConfig = ''
        reverse_proxy 10.100.0.50:8096 {
          header_up Host {host}
          header_up X-Forwarded-For {remote_host}
          header_up X-Real-IP {remote_host}
          header_up X-Forwarded-Proto {scheme}
        }
      '';

      # Audiobookshelf audiobook server (proxied to aion)
      "audiobookshelf.sbr.pm".extraConfig = ''
        reverse_proxy 10.100.0.49:13378 {
          header_up Host {host}
          header_up X-Forwarded-For {remote_host}
          header_up X-Real-IP {remote_host}
          header_up X-Forwarded-Proto {scheme}
        }
      '';

      # Webhook/gosmee service with SSE support
      "webhook.sbr.pm".extraConfig = ''
        reverse_proxy localhost:3333 {
          flush_interval -1
        }
      '';

      # Personal website with directory browsing
      "vincent.demeester.fr".extraConfig = ''
        root * /var/www/vincent.demeester.fr

        # Try files with .html extension
        try_files {path} {path}.html {path}/ /index.html

        file_server browse {
          hide .fancyindex README.md HEADER.md
        }

        ${securityHeaders}
      '';
    };
  };

  services.govanityurl = {
    enable = true;
    user = "caddy";
    host = "go.sbr.pm";
    config = ''
      paths:
        /lord:
          repo: https://github.com/vdemeester/lord
        /ape:
          repo: https://git.sr.ht/~vdemeester/ape
        /nr:
          repo: https://git.sr.ht/~vdemeester/nr
        /ram:
          repo: https://git.sr.ht/~vdemeester/ram
        /sec:
          repo: https://git.sr.ht/~vdemeester/sec
    '';
  };
  security.acme = {
    acceptTerms = true;
    defaults.email = "vincent@sbr.pm";
  };
}
