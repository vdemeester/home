{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    htop
    vim
  ];

  programs.zsh.enable = true;
}
