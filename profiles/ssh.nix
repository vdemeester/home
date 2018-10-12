{ config, pkgs, ... }:

{
  services = {
    openssh = {
      enable = true;
      startWhenNeeded = false;
      extraConfig = with import ../assets/machines.nix; ''
Host kerkouane kerkouane.sbr.pm
  Hostname kerkouane.sbr.pm
  Port ${toString ssh.kerkouane.port}
      '';
    };
  };
  programs.mosh.enable = true;
}
