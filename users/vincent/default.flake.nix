{ config, lib, pkgs, ... }:
with lib;
let
  secretPath = ../../secrets/machines.nix;
  secretCondition = (builtins.pathExists secretPath);

  isAuthorized = p: builtins.isAttrs p && p.authorized or false;
  authorizedKeys = lists.optionals secretCondition (
    attrsets.mapAttrsToList
      (name: value: value.key)
      (attrsets.filterAttrs (name: value: isAuthorized value) (import secretPath).ssh)
  );

  hasConfigVirtualizationContainers = builtins.hasAttr "containers" config.virtualisation;
  isContainersEnabled = if hasConfigVirtualizationContainers then config.virtualisation.containers.enable else false;
in
{
  users.users.vincent = {
    createHome = true;
    uid = 1000;
    description = "Vincent Demeester";
    extraGroups = [ "wheel" "input" ]
      ++ optionals config.profiles.desktop.enable [ "audio" "video" "networkmanager" ]
      ++ optionals config.profiles.scanning.enable [ "lp" "scanner" ]
      ++ optionals config.networking.networkmanager.enable [ "networkmanager" ]
      ++ optionals config.profiles.docker.enable [ "docker" ]
      ++ optionals config.virtualisation.buildkitd.enable [ "buildkit" ]
      ++ optionals config.profiles.virtualization.enable [ "libvirtd" ];
    shell = mkIf config.programs.zsh.enable pkgs.zsh;
    isNormalUser = true;
    openssh.authorizedKeys.keys = authorizedKeys;
    # FIXME change this ?
    initialPassword = "changeMe";
    # FIXME This might be handled differently by programs.podman, …
    subUidRanges = [{ startUid = 100000; count = 65536; }];
    subGidRanges = [{ startGid = 100000; count = 65536; }];
  };


  /*
  security.pam.services.vincent.fprintAuth = config.services.fprintd.enable;

  home-manager.users.vincent = lib.mkMerge
    (
      [
        (import ./core)
        (import ./mails { hostname = config.networking.hostName; pkgs = pkgs; })
      ]
      ++ optionals config.profiles.dev.enable [ (import ./dev) ]
      ++ optionals config.profiles.desktop.enable [ (import ./desktop) ]
      ++ optionals config.profiles.desktop.gnome.enable [ (import ./desktop/gnome.nix) ]
      ++ optionals config.profiles.desktop.i3.enable [ (import ./desktop/i3.nix) ]
      ++ optionals (config.networking.hostName == "wakasu") [
        {
          home.packages = with pkgs; [
            libosinfo
            asciinema
            oathToolkit
          ];
        }
      ]
      ++ optionals (config.profiles.laptop.enable && config.profiles.desktop.enable) [
        {
          # FIXME move this in its own file
          programs.autorandr.enable = true;
        }
      ]
      ++ optionals config.profiles.docker.enable [
        {
          home.packages = with pkgs; [ docker docker-compose ];
        }
      ]
      ++ optionals (config.profiles.yubikey.enable && config.profiles.yubikey.u2f) [{
        home.file.".config/Yubico/u2f_keys".source = pkgs.mkSecret ../../secrets/u2f_keys;
      }]
      ++ optionals (isContainersEnabled && config.profiles.dev.enable) [ (import ./containers) ]
      ++ optionals config.profiles.kubernetes.enable [ (import ./containers/kubernetes.nix) ]
      ++ optionals config.profiles.openshift.enable [ (import ./containers/openshift.nix) ]
      ++ optionals config.profiles.tekton.enable [ (import ./containers/tekton.nix) ]
      ++ optionals config.profiles.redhat.enable [{
        home.file.".local/share/applications/redhat-vpn.desktop".source = ./redhat/redhat-vpn.desktop;
        home.packages = with pkgs; [ gnome3.zenity oathToolkit ];
      }]
    );
    */
}
