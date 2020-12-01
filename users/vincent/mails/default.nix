{ hostname, pkgs, ... }:
let
  sync = (hostname == "wakasu");
in
{
  imports = [ ../../modules ];
  profiles.mails = {
    enable = true;
    sync = false;
  };
  home.file.".gmailctl/config.jsonnet".source = ./config.jsonnet;
  home.file.".gmailctl/gmailctl.jsonnet".source = ./gmailctl.libsonnet;
  home.packages = with pkgs; [ gmailctl ];
}
