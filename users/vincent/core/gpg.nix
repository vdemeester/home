{ pkgs, lib, nixosConfig, ... }:

let
  pinentry = if (nixosConfig.modules.desktop.enable) then pkgs.pinentry-gnome3 else pkgs.pinentry-tty;
in
{
  home.packages = with pkgs; [ gnupg ];
  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      enableExtraSocket = true;
      defaultCacheTtlSsh = 7200;
      pinentryPackage = pinentry;
    };
  };
}

