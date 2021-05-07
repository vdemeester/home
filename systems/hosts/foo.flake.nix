{ pkgs, lib, ... }:

with lib;
let
  secretPath = ../../secrets/machines.nix;
  secretCondition = (builtins.pathExists secretPath);

  ip = strings.optionalString secretCondition (import secretPath).wireguard.ips."${hostname}";
  ips = lists.optionals secretCondition ([ "${ip}/24" ]);
  endpointIP = strings.optionalString secretCondition (import secretPath).wg.endpointIP;
  endpointPort = if secretCondition then (import secretPath).wg.listenPort else 0;
  endpointPublicKey = strings.optionalString secretCondition (import secretPath).wireguard.kerkouane.publicKey;
in
{
  modules = {
    desktop = {
      i3.enable = true;
    };
    editors = {
      default = "vim";
      vim.enable = true;
    };
    hardware = {
      bluetooth.enable = true;
      audio.enable = true;
      yubikey.enable = true;
    };
    shell = {
      direnv.enable = true;
      git.enable = true;
      gnupg.enable = true;
      tmux.enable = true;
      zsh.enable = true;
    };
    virtualisation = {
      libvirt.enable = true;
      libvirt.nested = true;
    };
  };
  profiles = {
    home.enable = true;
    redhat.enable = true;
    laptop.enable = true;
  };

  environment.systemPackages = with pkgs; [ tektoncd-cli nyxt ];

  virtualisation.podman.enable = true;
  virtualisation.containers = {
    enable = true;
    registries = {
      search = [ "registry.fedoraproject.org" "registry.access.redhat.com" "registry.centos.org" "docker.io" "quay.io" ];
    };
    policy = {
      default = [{ type = "insecureAcceptAnything"; }];
      transports = {
        docker-daemon = {
          "" = [{ type = "insecureAcceptAnything"; }];
        };
      };
    };
  };
}
