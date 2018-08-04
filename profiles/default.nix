{ config, pkgs, ... }:

{
  imports = [
    ./users.nix
    ./overlays.nix
  ];
  boot.loader.systemd-boot.enable = true;
  environment = {
    variables = {
      EDITOR = pkgs.lib.mkOverride 0 "vim";
    };
    systemPackages = with pkgs; [
      direnv
      file
      htop
      iotop
      lsof
      netcat
      nix-beautify
      psmisc
      pv
      tmux
      tree
      vim
      wget
    ];
  };
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "fr";
    defaultLocale = "en_US.UTF-8";
  };
  nix = {
    useSandbox = true;
    gc = {
      automatic = true;
      dates = "monthly";
      options = "--delete-older-than 30d";
    };
    # if hydra is down, don't wait forever
    extraOptions = ''
      connect-timeout = 20
      build-cores = 0
    '';
  };
  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };

  system = {
    stateVersion = "18.03";
  };
  systemd.services.nixos-update = {
    description = "NixOS Upgrade";
    unitConfig.X-StopOnRemoval = false;
    serviceConfig.Type = "oneshot";

    environment = config.nix.envVars //
    { inherit (config.environment.sessionVariables) NIX_PATH;
      HOME = "/root";
    };
    path = [ pkgs.gnutar pkgs.xz pkgs.git config.nix.package.out ];
    script = ''
      cd /etc/nixos/
      git pull --autostash --rebase
      nix-channel --update
    '';
    startAt = "weekly";
  };
}
