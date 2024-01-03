{ pkgs, lib, nixosConfig, ... }:

let
  pinentry = if (nixosConfig.modules.desktop.enable) then "gnome3" else "tty";
in
{
  home.packages = with pkgs; [ gnupg ];
  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      enableExtraSocket = true;
      defaultCacheTtlSsh = 7200;
      pinentryFlavor = pinentry;
    };
  };
}
