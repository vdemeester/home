{
  globals,
  lib,
  libx,
  pkgs,
  ...
}:
let
  # TODO: migrate this out of here
  nginxExtraConfig = ''
    expires 31d;
    add_header Cache-Control "public, max-age=604800, immutable";
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains";
    add_header X-Content-Type-Options "nosniff";
    add_header X-Frame-Options "SAMEORIGIN";
    add_header X-Content-Security-Policy "default-src 'self' *.sbr.pm *.sbr.systems *.demeester.fr";
    add_header X-XSS-Protection "1; mode=block";
  '';

  nginx = pkgs.nginxMainline.override (_old: {
    modules = with pkgs.nginxModules; [
      fancyindex
    ];
  });

  filesWWW = {
    enableACME = true;
    forceSSL = true;
    root = "/var/www/dl.sbr.pm";
    locations."/" = {
      index = "index.html";
      extraConfig = ''
        fancyindex on;
        fancyindex_localtime on;
        fancyindex_exact_size off;
        fancyindex_header "/.fancyindex/header.html";
        fancyindex_footer "/.fancyindex/footer.html";
        # fancyindex_ignore "examplefile.html";
        fancyindex_ignore "README.md";
        fancyindex_ignore "HEADER.md";
        fancyindex_ignore ".fancyindex";
        fancyindex_name_length 255;
      '';
    };
    locations."/private" = {
      extraConfig = ''
        auth_basic "Restricted";
        auth_basic_user_file /var/www/dl.sbr.pm/private/.htpasswd;
      '';
    };
    extraConfig = nginxExtraConfig;
  };
in
{
  imports = [
    ../common/services/prometheus-exporters-node.nix
    # ../common/services/syncthing.nix
  ];

  # TODO make it an option ? (otherwise I'll add it for all)
  users.users.vincent.linger = true;
  services.openssh = {
    listenAddresses = [
      {
        addr = builtins.head globals.machines.kerkouane.net.vpn.ips;
        port = 22;
      }
    ];
    openFirewall = lib.mkForce false;
    passwordAuthentication = false;
    permitRootLogin = "without-password";
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
  services.nginx = {
    enable = true;
    statusPage = true;
    package = nginx;
    recommendedGzipSettings = true;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    virtualHosts."dl.sbr.pm" = filesWWW;
    virtualHosts."files.sbr.pm" = filesWWW;
    virtualHosts."ntfy.sbr.pm" = {
      enableACME = true;
      forceSSL = true;

      locations."/" = {
        proxyPass = "http://127.0.0.1:8111";
        proxyWebsockets = true;
        # basicAuthFile = config.secrets.ntfy_password.decrypted;
      };
    };
    virtualHosts."paste.sbr.pm" = {
      enableACME = true;
      forceSSL = true;
      root = "/var/www/paste.sbr.pm";
      locations."/" = {
        index = "index.html";
      };
      extraConfig = nginxExtraConfig;
    };
    virtualHosts."go.sbr.pm" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:8080";
      };
      extraConfig = nginxExtraConfig;
    };
    virtualHosts."whoami.sbr.pm" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://10.100.0.8:80";
        extraConfig = ''
          proxy_set_header Host            $host;
          proxy_set_header X-Forwarded-For $remote_addr;
        '';
      };
    };
    virtualHosts."webhook.sbr.pm" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:3333";
        extraConfig = ''
          proxy_buffering off;
          proxy_cache off;
          proxy_set_header Host            $host;
          proxy_set_header X-Forwarded-For $remote_addr;
          proxy_set_header Connection "";
          proxy_http_version 1.1;
          chunked_transfer_encoding off;
        '';
      };
    };
    virtualHosts."sbr.pm" = {
      enableACME = true;
      forceSSL = true;
      root = "/var/www/sbr.pm";
      locations."/" = {
        index = "index.html";
      };
      extraConfig = nginxExtraConfig;
    };
    virtualHosts."sbr.systems" = {
      enableACME = true;
      forceSSL = true;
      root = "/var/www/sbr.systems";
      locations."/" = {
        index = "index.html";
      };
      extraConfig = nginxExtraConfig;
    };
    virtualHosts."vincent.demeester.fr" = {
      enableACME = true;
      forceSSL = true;
      root = "/var/www/vincent.demeester.fr";
      locations."/" = {
        index = "index.html";
        extraConfig = ''
          default_type text/html;
          try_files $uri $uri.html $uri/ = 404;
          fancyindex on;
          fancyindex_localtime on;
          fancyindex_exact_size off;
          fancyindex_header "/assets/.fancyindex/header.html";
          fancyindex_footer "/assets/.fancyindex/footer.html";
          # fancyindex_ignore "examplefile.html";
          fancyindex_ignore "README.md";
          fancyindex_ignore "HEADER.md";
          fancyindex_ignore ".fancyindex";
          fancyindex_name_length 255;
        '';
      };
      extraConfig = nginxExtraConfig;
    };
  };
  services.prometheus.exporters.nginx = {
    enable = true;
    port = 9001;
  };
  services.govanityurl = {
    enable = true;
    user = "nginx";
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
  security.pam.enableSSHAgentAuth = true;
  security.acme = {
    acceptTerms = true;
    email = "vincent@sbr.pm";
  };
}
