{ pkgs, lib, ... }:

with lib;
let
  hostname = "kerkouane";

  networkingConfigPath = ../networking.nix;
  hasNetworkingConfig = (builtins.pathExists networkingConfigPath);
  secretPath = ../secrets/machines.nix;
  secretCondition = (builtins.pathExists secretPath);

  sshPort = if secretCondition then (import secretPath).ssh.kerkouane.port else 22;

  sources = import ../nix/sources.nix;
in
{
  imports = [
    (sources.nixos + "/nixos/modules/profiles/qemu-guest.nix")
    #(pkgs + "/nixos/modules/profiles/qemu-guest.nix")
    #<nixpkgs/nixos/modules/profiles/qemu-guest.nix>
    ../modules
    (import ../users).vincent
    (import ../users).root
  ]
  # digitalocean specifics
  ++ optionals hasNetworkingConfig [ networkingConfigPath ];

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

    # FIXME remove the need for it
    users.enable = false;

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
            repo: https://gitlab.com/vdemeester/nr
          /ram:
            repo: https://git.sr.ht/~vdemeester/ram
          /sec:
            repo: https://gitlab.com/vdemeester/sec
      '';
    };
    nginx = {
      enable = true;
      virtualHosts."dl.sbr.pm" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/vincent/desktop/sites/dl.sbr.pm";
        locations."/" = {
          index = "index.html";
        };
      };
      virtualHosts."paste.sbr.pm" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/vincent/desktop/sites/paste.sbr.pm";
        locations."/" = {
          index = "index.html";
        };
      };
      virtualHosts."go.sbr.pm" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = { proxyPass = "http://127.0.0.1:8080"; };
      };
      virtualHosts."sbr.pm" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/vincent/desktop/sites/sbr.pm";
        locations."/" = {
          index = "index.html";
        };
      };
      virtualHosts."sbr.systems" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/vincent/desktop/sites/sbr.systems";
        locations."/" = {
          index = "index.html";
        };
      };
      virtualHosts."vincent.demeester.fr" = {
        enableACME = true;
        forceSSL = true;
        root = "/home/vincent/desktop/sites/vincent.demeester.fr";
        locations."/" = {
          index = "index.html";
        };
      };
    };
    openssh.ports = [ sshPort ];
    openssh.permitRootLogin = "without-password";
    syncthing.guiAddress = "127.0.0.1:8384";
  };
}
