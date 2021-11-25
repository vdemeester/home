{ hostname, pkgs, ... }:
let
  sync = (hostname == "aomi");
in
{
  imports = [ ../../modules ];
  profiles.mails = {
    enable = true;
    sync = true;
  };
  home.file.".gmailctl/config.jsonnet".source = ./config.jsonnet;
  home.file.".gmailctl/gmailctl.jsonnet".source = ./gmailctl.libsonnet;
  home.packages = with pkgs; [ gmailctl ];
}
