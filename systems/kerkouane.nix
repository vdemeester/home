{ pkgs, lib, ... }:

with lib;
let
  hostname = "kerkouane";

  networkingConfigPath = ../networking.nix;
  hasNetworkingConfig = (builtins.pathExists networkingConfigPath);
  secretPath = ../secrets/machines.nix;
  secretCondition = (builtins.pathExists secretPath);

  sshPort = if secretCondition then (import secretPath).ssh.kerkouane.port else 22;

  nginxExtraConfig = ''
    expires 31d;
    add_header Cache-Control "public, max-age=604800, immutable";
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains";
    add_header X-Content-Type-Options "nosniff";
    add_header X-Frame-Options "SAMEORIGIN";
    add_header X-Content-Security-Policy "default-src 'self' *.sbr.pm *.sbr.systems";
    add_header X-XSS-Protection "1; mode=block";
  '';

  sources = import ../nix/sources.nix;
in
{
  imports = [
    (sources.nixos + "/nixos/modules/profiles/qemu-guest.nix")
    ./modules
    (import ../users).vincent
    (import ../users).root
  ]
  # digitalocean specifics
  ++ optionals hasNetworkingConfig [ networkingConfigPath ];

  networking.hostName = hostname;

  boot.loader.grub.device = "/dev/vda";
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
    syncthing.enable = true;
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
      '';
    };
    nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      virtualHosts."dl.sbr.pm" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/vincent/desktop/sites/dl.sbr.pm";
        locations."/" = {
          index = "index.html";
        };
        extraConfig = nginxExtraConfig;
      };
      virtualHosts."paste.sbr.pm" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/vincent/desktop/sites/paste.sbr.pm";
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
        root = "/home/vincent/desktop/sites/sbr.pm";
        locations."/" = {
          index = "index.html";
        };
        extraConfig = nginxExtraConfig;
      };
      virtualHosts."sbr.systems" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/vincent/desktop/sites/sbr.systems";
        locations."/" = {
          index = "index.html";
        };
        extraConfig = nginxExtraConfig;
      };
      virtualHosts."vincent.demeester.fr" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/vincent/desktop/sites/vincent.demeester.fr";
        locations."/" = {
          index = "index.html";
        };
        extraConfig = nginxExtraConfig;
      };
    };
    openssh.ports = [ sshPort ];
    openssh.permitRootLogin = "without-password";
    syncthing.guiAddress = "127.0.0.1:8384";
  };
}
