{ hostname, ... }:
let
  sync = (hostname == "wakasu");
in
{
  imports = [ ../../modules ];
  profiles.mails = {
    enable = true;
    sync = sync;
  };
}
