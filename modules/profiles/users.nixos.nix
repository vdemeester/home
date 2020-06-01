{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.users;
  secretPath = ../../secrets/machines.nix;
  secretCondition = (builtins.pathExists secretPath);
  machines = optionalAttrs secretCondition (import secretPath);

  isAuthorized = p: builtins.isAttrs p && p.authorize or false;
  authorizedKeys = lists.optional secretCondition (
    attrsets.mapAttrsToList
      (name: value: value.key)
      (attrsets.filterAttrs (name: value: isAuthorized value) machines.ssh)
  );
in
{
  options = {
    profiles.users = {
      enable = mkOption {
        default = true;
        description = "Enable users profile";
        type = types.bool;
      };
      user = mkOption {
        default = "vincent";
        description = "Username to use when creating user";
        type = types.str;
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      users = {
        extraUsers = {
          ${cfg.user} = {
            isNormalUser = true;
            uid = 1000;
            createHome = true;
            extraGroups = [ "wheel" "input" ] ++ optionals config.profiles.desktop.enable [ "audio" "video" "lp" "scanner" "networkmanager" ]
              ++ optionals config.profiles.docker.enable [ "docker" ]
              ++ optionals config.profiles.buildkit.enable [ "buildkit" ]
              ++ optionals config.profiles.virtualization.enable [ "libvirtd" "vboxusers" ];
            shell = if config.programs.fish.enable then pkgs.fish else pkgs.zsh;
            initialPassword = "changeMe";
            subUidRanges = [{ startUid = 100000; count = 65536; }];
            subGidRanges = [{ startGid = 100000; count = 65536; }];
            openssh.authorizedKeys.keys = authorizedKeys;
          };
        };
      };
    }
    (
      mkIf secretCondition {
        programs.ssh.extraConfig = with import ../../secrets/machines.nix; ''
          Host kerkouane kerkouane.sbr.pm
            Hostname kerkouane.sbr.pm
            Port ${toString ssh.kerkouane.port}
          Host kerkouane.vpn ${wireguard.ips.kerkouane}
            Hostname ${wireguard.ips.kerkouane}
            Port ${toString ssh.kerkouane.port}
          Host carthage carthage.sbr.pm
            Hostname carthage.sbr.pm
            Port ${toString ssh.carthage.port}
          Host carthage.vpn ${wireguard.ips.carthage}
            Hostname ${wireguard.ips.carthage}
            Port ${toString ssh.carthage.port}
          Host hokkaido.vpn ${wireguard.ips.hokkaido}
            Hostname ${wireguard.ips.hokkaido}
          Host honshu.vpn ${wireguard.ips.honshu}
            Hostname ${wireguard.ips.honshu}
          Host okinawa.vpn ${wireguard.ips.okinawa}
            Hostname ${wireguard.ips.okinawa}
          Host wakasu.vpn ${wireguard.ips.wakasu}
            Hostname ${wireguard.ips.wakasu}
        '';
      }
    )
  ]);
}
