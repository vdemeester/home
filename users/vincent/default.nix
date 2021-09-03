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
  warnings = if (versionAtLeast config.system.nixos.release "21.11") then [ ] else [ "NixOS release: ${config.system.nixos.release}" ];
  users.users.vincent = {
    createHome = true;
    uid = 1000;
    description = "Vincent Demeester";
    extraGroups = [ "wheel" "input" ]
      ++ optionals config.profiles.desktop.enable [ "audio" "video" "networkmanager" ]
      ++ optionals config.profiles.scanning.enable [ "lp" "scanner" ]
      ++ optionals config.networking.networkmanager.enable [ "networkmanager" ]
      ++ optionals config.virtualisation.docker.enable [ "docker" ]
      ++ optionals config.virtualisation.buildkitd.enable [ "buildkit" ]
      ++ optionals config.profiles.virtualization.enable [ "libvirtd" ];
    shell = mkIf config.programs.zsh.enable pkgs.zsh;
    isNormalUser = true;
    openssh.authorizedKeys.keys = authorizedKeys;
    initialPassword = "changeMe";
    subUidRanges = [{ startUid = 100000; count = 65536; }];
    subGidRanges = [{ startGid = 100000; count = 65536; }];
  };

  nix = {
    trustedUsers = [ "vincent" ];
    sshServe.keys = authorizedKeys;
  };

  security = {
    pam = {
      # Nix will hit the stack limit when using `nixFlakes`.
      loginLimits = [
        { domain = config.users.users.vincent.name; item = "stack"; type = "-"; value = "unlimited"; }
      ];
    };
  };

  # Enable user units to persist after sessions end.
  system.activationScripts.loginctl-enable-linger-vincent = lib.stringAfter [ "users" ] ''
    ${pkgs.systemd}/bin/loginctl enable-linger ${config.users.users.vincent.name}
  '';

  # To use nixos config in home-manager configuration, use the nixosConfig attr.
  # This make it possible to import the whole configuration, and let each module
  # load their own.
  home-manager.users.vincent = lib.mkMerge
    (
      [
        (import ./core)
        (import ./mails { hostname = config.networking.hostName; pkgs = pkgs; })
        (import ./containers/kubernetes.nix)
        (import ./containers/openshift.nix)
        (import ./containers/tekton.nix)
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
      ++ optionals config.profiles.redhat.enable [{
        home.file.".local/share/applications/redhat-vpn.desktop".source = ./redhat/redhat-vpn.desktop;
        home.packages = with pkgs; [ gnome3.zenity oathToolkit ];
      }]
      ++ optionals (versionOlder config.system.nixos.release "21.11") [{
        # FIXME manpages are broken on 21.05 and home-manager (for some reason..)
        manual.manpages.enable = false;
      }]
    );
}
