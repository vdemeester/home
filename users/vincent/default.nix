{ config, lib, pkgs, ... }:
with lib;
{
  users.users.vincent = {
    createHome = true;
    uid = 1000;
    description = "Vincent Demeester";
    extraGroups = [ "wheel" "input" ]
      ++ optionals config.profiles.desktop.enable [ "audio" "video" "lp" "scanner" "networkmanager" ]
      ++ optionals config.networking.networkmanager.enable [ "networkmanager" ]
      ++ optionals config.profiles.docker.enable [ "docker" ]
      ++ optionals config.profiles.buildkit.enable [ "buildkit" ]
      ++ optionals config.profiles.virtualization.enable [ "libvirtd" ];
    shell = mkIf config.programs.zsh.enable pkgs.zsh;
    isNormalUser = true;
    # FIXME handle this too
    openssh.authorizedKeys.keys = [ ];
    # FIXME change this ?
    initialPassword = "changeMe";
    # FIXME This might be handled differently by programs.podman, …
    subUidRanges = [{ startUid = 100000; count = 65536; }];
    subGidRanges = [{ startGid = 100000; count = 65536; }];
  };

  virtualisation.containers.users = [ "vincent" ];
  security.pam.services.vincent.fprintAuth = config.services.fprintd.enable;

  home-manager.users.vincent = lib.mkMerge (
    [
      (import ../modules)
      (import ./core)
    ]
    ++ optionals config.profiles.dev.enable [ (import ./dev) ]
    ++ optionals config.profiles.desktop.enable [ (import ./desktop) ]
    ++ optionals config.profiles.docker.enable [{
      home.packages = with pkgs; [ docker docker-compose ];
    }]
    ++ optionals (config.virtualisation.containers.enable && config.profiles.dev.enable) [
      (import ./containers)
    ]
    # TODO: mpd
  );
}