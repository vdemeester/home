{ lib, pkgs, ... }:
{
  services.openssh.enable = true;
  services.openssh.settings = {
    PermitRootLogin = "yes";
    PasswordAuthentication = true;
  };

  networking.useDHCP = lib.mkDefault true;

  environment.systemPackages = with pkgs; [
    vim
    git
    htop
    helix
    # Add any additional packages you need.
  ];

}
