{
  pkgs,
  lib,
  config,
  desktop,
  hostname,
  outputs,
  stateVersion,
  inputs,
  globals,
  libx,
  ...
}:
let
  ifExists = groups: builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
in
{
  users.users.vincent = {
    description = "Vincent Demeester";
    createHome = true;
    uid = 1000;
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups =
      [
        "users"
        "wheel"
      ]
      ++ lib.optionals (builtins.isString desktop) [
        "networkmanager"
        "audio"
        "video"
      ]
      ++ ifExists [
        "buildkit"
        "docker"
        "libvirt"
        "libvirtd"
        "lxd"
        "nginx"
        "plugdev"
        "tss"
        "messagebus"
        "lp"
        "scanner"
      ];
    subUidRanges = [
      {
        startUid = 100000;
        count = 65536;
      }
    ];
    subGidRanges = [
      {
        startGid = 100000;
        count = 65536;
      }
    ];
    initialPassword = "changeMe";

    # FIXME set this up better
    openssh.authorizedKeys.keys = globals.ssh.vincent;

    # 🤔
    packages = [ pkgs.home-manager ];
  };

  nix.settings.trusted-users = [ "vincent" ];

  security = {
    pam = {
      # Nix will hit the stack limit when using `nixFlakes`.
      loginLimits = [
        {
          domain = config.users.users.vincent.name;
          item = "stack";
          type = "-";
          value = "unlimited";
        }
      ];
    };
  };

  # Enable user units to persist after sessions end.
  # system.activationScripts.loginctl-enable-linger-vincent = lib.stringAfter [ "users" ] ''
  #   ${pkgs.systemd}/bin/loginctl enable-linger ${config.users.users.vincent.name}
  # '';

  # Do I user home-manager nixosModule *or* home-manager on its own
  home-manager.users.vincent = import ../../../home/default.nix {
    inherit
      config
      pkgs
      lib
      hostname
      desktop
      globals
      outputs
      inputs
      stateVersion
      libx
      ;
    username = "vincent";
  };
  # This is a workaround for not seemingly being able to set $EDITOR in home-manager
  environment.sessionVariables = {
    EDITOR = "emacs";
  };
}
