{ pkgs, lib, nixosConfig, ... }:

let
  stable = lib.versionOlder nixosConfig.system.nixos.release "24.05";
in
{
  home.packages = with pkgs; [ gnupg ];
  programs.gpg = {
    enable = true;

    # https://support.yubico.com/hc/en-us/articles/4819584884124-Resolving-GPG-s-CCID-conflicts
    scdaemonSettings = {
      disable-ccid = true;
    };
  };
  services = {
    gpg-agent = {
      enable = true;
      # enableSshSupport = true;
      enableExtraSocket = true;
      # defaultCacheTtlSsh = 7200;
    } // (if stable then {
      pinentryFlavor = if (nixosConfig.modules.desktop.enable) then "gnome3" else "tty";
    } else {
      pinentryPackage = if (nixosConfig.modules.desktop.enable) then pkgs.pinentry-gnome3 else pkgs.pinentry-tty;
    });
  };
}
