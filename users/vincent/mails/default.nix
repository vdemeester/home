{ hostname, pkgs, ... }:
let
  sync = (hostname == "wakasu");
in
{
  imports = [ ../../modules ];
  profiles.mails = {
    enable = true;
    sync = sync;
  };
  home.file.".gmailctl/config.jsonnet".source = ./config.jsonnet;
  home.packages = with pkgs; [ gmailctl ];
}
