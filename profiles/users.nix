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
          with import ../assets/ssh-keys.nix; [ honshu wakasu hokkaido california shikoku massimo carthage ];
          subUidRanges = [{ startUid = 100000; count = 65536; }];
          subGidRanges = [{ startGid = 100000; count = 65536; }];
      };
    };
    };
    programs.ssh.extraConfig = with import ../assets/machines.nix; ''
Host kerkouane kerkouane.sbr.pm ${wireguard.ips.kerkouane}
  Hostname kerkouane.sbr.pm
  Port ${toString ssh.kerkouane.port}
Host carthage carthage.sbr.pm ${wireguard.ips.carthage}
  Hostname carthage.sbr.pm
  Port ${toString ssh.carthage.port}
    '';
}
