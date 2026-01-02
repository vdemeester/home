{
  globals,
  lib,
  libx,
  pkgs,
  ...
}:
let
  # Common security headers for Caddy
  securityHeaders = ''
    header {
      Strict-Transport-Security "max-age=31536000; includeSubDomains; preload"
      X-Content-Type-Options "nosniff"
      X-Frame-Options "SAMEORIGIN"
      Referrer-Policy "strict-origin-when-cross-origin"
      Permissions-Policy "geolocation=(), microphone=(), camera=(), payment=(), usb=(), magnetometer=(), gyroscope=(), accelerometer=()"
      Content-Security-Policy "default-src 'self' *.sbr.pm *.demeester.fr"
      X-XSS-Protection "1; mode=block"
      Cache-Control "public, max-age=604800, immutable"
      -Server
    }
  '';

  # Security headers for media services (more permissive CSP for multimedia)
  mediaSecurityHeaders = ''
    header {
      Strict-Transport-Security "max-age=31536000; includeSubDomains; preload"
      X-Content-Type-Options "nosniff"
      X-Frame-Options "SAMEORIGIN"
      Referrer-Policy "strict-origin-when-cross-origin"
      Permissions-Policy "geolocation=(), microphone=(), camera=(), payment=(), usb=()"
      -Server
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

    # Use Caddy with rate-limit plugin
    package = pkgs.caddy.withPlugins {
      plugins = [ "github.com/mholt/caddy-ratelimit@v0.1.1-0.20250915152450-04ea34edc0c4" ];
      hash = "sha256-U4kx1A6UtA/VjqFWMD/hliH8QCeOqUsr9Y/X4xWggbQ=";
    };

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
        # Rate limiting for notification service
        rate_limit {
          zone ntfy_publish {
            key {remote_host}
            events 50
            window 1m
          }
        }

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
        }
      '';

      # Immich photo management (proxied to rhea)
      "immich.sbr.pm".extraConfig = ''
        # Allow large photo/video uploads (50GB limit)
        request_body {
          max_size 50GB
        }

        # Strict rate limiting for authentication endpoints
        @auth {
          path /auth/* /api/auth/*
        }
        route @auth {
          rate_limit {
            zone immich_auth {
              key {remote_host}
              events 10
              window 1m
            }
          }
          reverse_proxy 10.100.0.50:2283 {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
          }
        }

        # Moderate rate limiting for API endpoints
        @api {
          path /api/*
        }
        route @api {
          rate_limit {
            zone immich_api {
              key {remote_host}
              events 100
              window 1m
            }
          }
          reverse_proxy 10.100.0.50:2283 {
            header_up Host {host}
            header_up X-Real-IP {remote_host}
          }
        }

        # Permissive rate limiting for media/general requests
        rate_limit {
          zone immich_media {
            key {remote_host}
            events 1000
            window 1m
          }
        }

        reverse_proxy 10.100.0.50:2283 {
          header_up Host {host}
          header_up X-Real-IP {remote_host}
        }

        ${mediaSecurityHeaders}
      '';

      # Navidrome music streaming (proxied to aion)
      "navidrome.sbr.pm".extraConfig = ''
        # Rate limiting for music streaming
        rate_limit {
          zone navidrome_general {
            key {remote_host}
            events 500
            window 1m
          }
        }

        reverse_proxy 10.100.0.49:4533 {
          header_up Host {host}
          header_up X-Real-IP {remote_host}
        }

        ${mediaSecurityHeaders}
      '';

      # Jellyfin media server (proxied to rhea)
      "jellyfin.sbr.pm".extraConfig = ''
        # Rate limiting for media server
        rate_limit {
          zone jellyfin_general {
            key {remote_host}
            events 500
            window 1m
          }
        }

        reverse_proxy 10.100.0.50:8096 {
          header_up Host {host}
          header_up X-Real-IP {remote_host}
        }

        ${mediaSecurityHeaders}
      '';

      # Audiobookshelf audiobook server (proxied to aion)
      "audiobookshelf.sbr.pm".extraConfig = ''
        # Rate limiting for audiobook streaming
        rate_limit {
          zone audiobookshelf_general {
            key {remote_host}
            events 500
            window 1m
          }
        }

        reverse_proxy 10.100.0.49:13378 {
          header_up Host {host}
          header_up X-Real-IP {remote_host}
        }

        ${mediaSecurityHeaders}
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
