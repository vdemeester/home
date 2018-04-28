{ config, pkgs, ... }:

{
  imports = [
    ./users.nix
    ../pkgs/home-manager/nixos
  ];
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
      psmisc
      tmux
      tree
      vim
      wget
      nix-beautify
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
    overlays = [
      (import ../overlays/sbr.overlay.nix)
      # add third-party packages from outside the nixpkgs tree
      (self: super: {
        nix-beautify = import ../pkgs/nix-beautify { inherit pkgs; };
        home-manager = import ../pkgs/home-manager { inherit pkgs; };
      })
    ];
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
