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
  sops.secrets.u2f_keys = mkIf (config.profiles.yubikey.enable && config.profiles.yubikey.u2f) {
    path = "/home/vincent/.config/Yubico/u2f_keys";
    owner = "vincent";
  };
  users.users.vincent = {
    createHome = true;
    uid = 1000;
    description = "Vincent Demeester";
    extraGroups = [ "wheel" "input" ]
      ++ optionals config.networking.networkmanager.enable [ "networkmanager" ]
      ++ optionals config.profiles.desktop.enable [ "audio" "video" ]
      ++ optionals config.profiles.scanning.enable [ "lp" "scanner" ]
      ++ optionals config.networking.networkmanager.enable [ "networkmanager" ]
      ++ optionals config.virtualisation.docker.enable [ "docker" ]
      ++ optionals config.virtualisation.buildkitd.enable [ "buildkit" ]
      ++ optionals config.profiles.virtualization.enable [ "libvirtd" ]
      ++ optionals config.services.nginx.enable [ "nginx" ];
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
  # FIXME(vdemeester) using nixosConfig, we can get the NixOS configuration from
  # the home-manager configuration. This should help play around the conditions
  # inside each "home-manager" modules instead of here.
  home-manager.users.vincent = lib.mkMerge
    (
      [
        (import ./core)
        (import ./mails { hostname = config.networking.hostName; pkgs = pkgs; })
      ]
      ++ optionals config.profiles.dev.enable [
        (import ./dev)
        (import ./containers/kubernetes.nix)
        (import ./containers/openshift.nix)
        (import ./containers/tekton.nix)
      ]
      ++ optionals config.profiles.desktop.enable [ (import ./desktop) ]
      ++ optionals config.profiles.desktop.gnome.enable [ (import ./desktop/gnome.nix) ]
      # ++ optionals config.profiles.desktop.i3.enable [ (import ./desktop/i3.nix) ]
      ++ optionals (config.networking.hostName == "wakasu") [
        {
          home.packages = with pkgs; [
            libosinfo
            asciinema
            oathToolkit
          ];
        }
      ]
      ++ optionals config.profiles.docker.enable [
        {
          home.packages = with pkgs; [ docker docker-compose ];
        }
      ]
      ++ optionals (isContainersEnabled && config.profiles.dev.enable) [ (import ./containers) ]
      ++ optionals config.profiles.redhat.enable [{
        home.file.".local/share/applications/redhat-vpn.desktop".source = ./redhat/redhat-vpn.desktop;
        home.packages = with pkgs; [ gnome3.zenity oathToolkit ];
      }]
    );
}
