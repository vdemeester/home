{ pkgs, lib, ... }:

with lib;
let
  hostname = "kerkouane";

  secretPath = ../../secrets/machines.nix;
  secretCondition = (builtins.pathExists secretPath);

  wireguardIp = strings.optionalString secretCondition (import secretPath).wireguard.ips."${hostname}";

  nginxExtraConfig = ''
    expires 31d;
    add_header Cache-Control "public, max-age=604800, immutable";
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains";
    add_header X-Content-Type-Options "nosniff";
    add_header X-Frame-Options "SAMEORIGIN";
    add_header X-Content-Security-Policy "default-src 'self' *.sbr.pm *.sbr.systems *.demeester.fr";
    add_header X-XSS-Protection "1; mode=block";
  '';

  nginx = pkgs.nginxMainline.override (old: {
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

  sources = import ../../nix/sources.nix;
in
{
  imports = [
    (sources.nixos + "/nixos/modules/profiles/qemu-guest.nix")
    (import ../../nix).home-manager-stable
    ../modules
    (import ../../users).vincent
    (import ../../users).root
  ];

  networking.hostName = hostname;

  # START OF DigitalOcean specifics
  # FIXME: move this into a secret ?
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [
      "67.207.67.2"
      "67.207.67.3"
    ];
    defaultGateway = "188.166.64.1";
    defaultGateway6 = "";
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce true;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          { address = "188.166.102.243"; prefixLength = 18; }
          { address = "10.18.0.5"; prefixLength = 16; }
        ];
        ipv6.addresses = [
          { address = "fe80::8035:3aff:fe72:1036"; prefixLength = 64; }
        ];
      };

    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="82:35:3a:72:10:36", NAME="eth0"

  '';
  # END OF DigitalOcean specifics

  boot.loader.grub.device = "/dev/vda";
  boot.loader.grub.enable = lib.mkForce true;
  boot.loader.systemd-boot.enable = lib.mkForce false;
  fileSystems."/" = { device = "/dev/vda1"; fsType = "ext4"; };
  swapDevices = [{ device = "/swapfile"; size = 1024; }];

  core.nix = {
    # FIXME move this away
    localCaches = [ ];
    buildCores = 1;
  };

  profiles = {
    git.enable = true;
    ssh.enable = true;
    wireguard.server.enable = true;
  };

  networking.firewall.allowPing = true;
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  security = {
    acme = {
      acceptTerms = true;
      email = "vincent@sbr.pm";
    };
    #acme.certs = {
    #  "sbr.pm".email = "vincent@sbr.pm";
    #};
  };
  security.pam.enableSSHAgentAuth = true;
  #systemd.services.nginx.serviceConfig.ReadWritePaths = [ "/var/www" ];
  systemd.services.nginx.serviceConfig.ProtectHome = lib.mkForce false;
  services = {
    govanityurl = {
      enable = true;
      user = "nginx";
      host = "go.sbr.pm";
      config = ''
        paths:
          /ape:
            repo: https://git.sr.ht/~vdemeester/ape
          /nr:
            repo: https://git.sr.ht/~vdemeester/nr
          /ram:
            repo: https://git.sr.ht/~vdemeester/ram
          /sec:
            repo: https://git.sr.ht/~vdemeester/sec
         /foo
            repo: https://git.sr.ht/~vdemeester/foo
      '';
    };
    nginx = {
      enable = true;
      package = nginx;
      recommendedGzipSettings = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      virtualHosts."dl.sbr.pm" = filesWWW;
      virtualHosts."files.sbr.pm" = filesWWW;
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
        locations."/" = { proxyPass = "http://127.0.0.1:8080"; };
        extraConfig = nginxExtraConfig;
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
    openssh = {
      listenAddresses = [
        { addr = wireguardIp; port = 22; }
      ];
      openFirewall = false;
      passwordAuthentication = false;
      permitRootLogin = "without-password";
    };
  };
}
