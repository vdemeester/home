{ ... }:
{
  services.fprintd = {
    enable = true;
  };
  security.pam.services = {
    login.fprintAuth = true;
    sudo.fprintAuth = true;
  };
}
