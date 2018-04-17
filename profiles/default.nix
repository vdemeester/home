{ config, pkgs, ... }:

{
  imports =
    [
    ./users.nix
  ];
  programs = {
    zsh.enable = true;
    fish.enable = true;
  };
  environment = {
    variables = {
      EDITOR = pkgs.lib.mkOverride 0 "vim";
    };
    systemPackages = with pkgs; [
      aspell
      aspellDicts.en
      aspellDicts.fr
      cryptsetup
      direnv
      file
      htop
      iotop
      lsof
      netcat
      psmisc
      tmux
      tree
      vim
      wget
      fish
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
/*
    nixPath = [
      "/nix/var/nix/profiles/per-user/root/channels/nixos"
      "nixpkgs-unstable=/nix/var/nix/profiles/per-user/root/channels/nixpkgs"
      #"sbrpkgs=/etc/nixos/pkgsets/sbr"
      "nixos-config=/etc/nixos/configuration.nix"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
*/
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
