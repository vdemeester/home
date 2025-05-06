{
  pkgs,
  ...
}:
{
  virtualisation = {
    lxd = {
      enable = true;
      # https://documentation.ubuntu.com/lxd/en/latest/howto/access_ui/
      ui = {
        enable = true;
        package = pkgs.lxd-ui;
      };
    };
  };

  networking = {
    firewall = {
      trustedInterfaces = [ "lxdbr0" ];
    };
  };
}
