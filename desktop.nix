{ pkgs, prefix, ... }:

{
  imports = [ ./base.nix ];
  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      defaultCacheTtlSsh = 7200;
      extraConfig = ''
      allow-emacs-pinentry
      '';
    };
  };
  programs = {
    firefox.enable = true;
  };
  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.fr
    #etBook
    keybase
    peco
    spotify
    youtube-dl
  ];
}
