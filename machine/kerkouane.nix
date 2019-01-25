{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ../networking.nix ];
  time.timeZone = "Europe/Paris";
  boot = {
    cleanTmpDir = true;
    loader.grub.enable = true;
  };
  locations."/public/default/index" =
  let file = pkgs.writeText "index" "Welcome !";
  in 
  { alias = file;
    extraConfig = ''
      etag off;
      add_header etag "\"${builtins.substring 11 32 file.outPath}\"";
      '';
  }
  profiles = {
    git.enable = true;
    nix-config.localCaches = [];
    nix-config.buildCores = 1;
    nix-auto-update.autoUpgrade = false;
    ssh.enable = true;
    wireguard.server.enable = true;
  };
  networking.firewall.allowPing = true;
  services = {
    nginx = {
      enable = true;
      virtualHosts."sbr.pm" = {
        enableACME = true;
        forceSSL = true;
        root = "/public/default";
      };
    };
    openssh.ports = [ ssh.kerkouane.port ];
    openssh.permitRootLogin = "without-password";
  };
  security = {
    acme.certs = {
      "sbr.pm".email = "vincent@sbr.pm";
    };
  };
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGR4dqXwHwPpYgyk6yl9+9LRL3qrBZp3ZWdyKaTiXp0p vincent@shikoku"
  ];
}
