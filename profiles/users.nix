{ config, pkgs, ... }:

{
  users = {
    extraUsers = {
      vincent = {
        isNormalUser = true;
        uid = 1000;
        createHome = true;
        extraGroups = [ "networkmanager" "wheel" "docker" "buildkit" "vboxusers" "libvirtd" "input" "audio" "video" "scanner" ];
        shell = if config.programs.fish.enable then pkgs.fish else pkgs.bash;
        initialPassword = "changeMe";
        openssh.authorizedKeys.keys =
          with import ../assets/ssh-keys.nix; [ honshu kerkouane hokkaido california shikoku massimo carthage ];
          subUidRanges = [{ startUid = 100000; count = 65536; }];
          subGidRanges = [{ startGid = 100000; count = 65536; }];
      };
    };
    };
    programs.ssh.extraConfig = with import ../assets/machines.nix; ''
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
Host honshu.vpn ${wireguard.ips.honshu}
  Hostname ${wireguard.ips.honshu}
Host shikoku.vpn ${wireguard.ips.shikoku}
  Hostname ${wireguard.ips.shikoku}
Host hokkaido.vpn ${wireguard.ips.hokkaido}
  Hostname ${wireguard.ips.hokkaido}
Host massimo.vpn ${wireguard.ips.massimo}
  Hostname ${wireguard.ips.massimo}
    '';
}
