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
  imports = [
    ./desktop.flake.nix
    ./laptop.flake.nix
  ];
  users.users.vincent = {
    createHome = true;
    uid = 1000;
    description = "Vincent Demeester";
    extraGroups = [ "wheel" "input" ]
      ++ optionals config.profiles.desktop.enable [ "audio" "video" "networkmanager" ];
    #++ optionals config.profiles.scanning.enable [ "lp" "scanner" ]
    #++ optionals config.networking.networkmanager.enable [ "networkmanager" ]
    #++ optionals config.profiles.docker.enable [ "docker" ]
    #++ optionals config.virtualisation.buildkitd.enable [ "buildkit" ]
    #++ optionals config.profiles.virtualization.enable [ "libvirtd" ];
    shell = mkIf config.programs.zsh.enable pkgs.zsh;
    isNormalUser = true;
    openssh.authorizedKeys.keys = authorizedKeys;
    initialPassword = "changeMe";
    subUidRanges = [{ startUid = 100000; count = 65536; }];
    subGidRanges = [{ startGid = 100000; count = 65536; }];
  };

  home-manager.users.vincent = mkMerge ([
    (import ../modules)
  ]); # ++ optionals config.profiles.desktop.enable [ ./desktop ]);

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

}
