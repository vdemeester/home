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
}
