{ pkgs, ... }:

{
  home.packages = with pkgs; [ gnupg ];
  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      enableExtraSocket = true;
      defaultCacheTtlSsh = 7200;
      pinentryFlavor = "gnome3";
    };
  };
}
