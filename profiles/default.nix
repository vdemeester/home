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
      cachix
      direnv
      file
      htop
      iotop
      lsof
      netcat
      psmisc
      pv
      tmux
      tree
      vim
      vrsync
      wget
    ];
  };
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "fr-bepo";
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
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://r-ryantm.cachix.org"
      "https://vdemeester.cachix.org"
      "https://shortbrain.cachix.org"
    ];
    binaryCachePublicKeys = [
      "r-ryantm.cachix.org-1:gkUbLkouDAyvBdpBX0JOdIiD2/DP1ldF3Z3Y6Gqcc4c="
      "vdemeester.cachix.org-1:uCECG6so7v1rs77c5NFz2dCePwd+PGNeZ6E5DrkT7F0="
      "shortbrain.cachix.org-1:dqXcXzM0yXs3eo9ChmMfmob93eemwNyhTx7wCR4IjeQ="
    ];
    trustedUsers = [ "root" "vincent" ];
  };
  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };

  system = {
    stateVersion = "18.09";
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
