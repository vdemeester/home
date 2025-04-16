{ pkgs, ... }:
{
  environment = {
    # Install some packages
    systemPackages = with pkgs; [
      git
    ];
    # Default gitconfig
    etc."gitconfig".source = ./git/config;
    etc."gitignore".source = ./git/ignore;
  };
}
