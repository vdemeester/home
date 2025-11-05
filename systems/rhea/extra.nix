{ pkgs, ... }:
{
  services.openssh.enable = true;
  services.openssh.settings = {
    PermitRootLogin = "yes";
    PasswordAuthentication = true;
  };

  environment.systemPackages = with pkgs; [
    vim
    git
    htop
    helix
    # Add any additional packages you need.
  ];

}
