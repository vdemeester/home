{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.mails;
in
{
  options = {
    profiles.mails = {
      enable = mkOption {
        default = true;
        description = "Enable mails configurations";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    accounts.email = {
      maildirBasePath = "desktop/mails";
      accounts = {
        "redhat" = {
          address = "vdemeest@redhat.com";
          userName = "vdemeest@redhat.com";
          realName = "Vincent Demeester";
          passwordCommand = "${pkgs.gnupg}/bin/gpg -q --for-your-eyes-only --no-tty --exit-on-status-write-error --batch --passphrase-file ~/sync/rh.pass -d ~/desktop/documents/rh.pass.gpg";
          imap.host = "imap.gmail.com";
          smtp.host = "smtp.gmail.com";
          mbsync = {
            enable = true;
            create = "both";
            #expunge = "both";
            patterns = ["*" "![Gmail]*" "[Gmail]/Sent Mail" "[Gmail]/Starred" "[Gmail]/All Mail"];
            extraConfig = {
              channel = {
                Sync = "All";
              };
            };
          };
          notmuch.enable = true;
          astroid.enable = true;
          msmtp.enable = true;
        };
        "perso" = {
          primary = true;
          address = "vinc.demeester@gmail.com";
          userName = "vinc.demeester@gmail.com";
          realName = "Vincent Demeester";
          passwordCommand = "${pkgs.gnupg}/bin/gpg -q --for-your-eyes-only --no-tty --exit-on-status-write-error --batch --passphrase-file ~/sync/perso.pass -d ~/desktop/documents/perso.pass.gpg";
          imap.host = "imap.gmail.com";
          smtp.host = "smtp.gmail.com";
          mbsync = {
            enable = true;
            create = "both";
            #expunge = "both";
            patterns = ["*" "![Gmail]*" "[Gmail]/Sent Mail" "[Gmail]/Starred" "[Gmail]/All Mail"];
            extraConfig = {
              channel = {
                Sync = "All";
              };
            };
          };
          notmuch.enable = true;
          astroid.enable = true;
          msmtp.enable = true;
        };
      };
    };
    services.mbsync = {
      enable = true;
      preExec = "${pkgs.coreutils}/bin/mkdir -p /home/vincent/desktop/mails/redhat /home/vincent/desktop/mails/perso";
      postExec = "env NOTMUCH_CONFIG=/home/vincent/.config/notmuch/notmuchrc NMBGIT=/home/vincent/.local/share/notmuch/nmbug ${pkgs.notmuch}/bin/notmuch new";
    };
    programs.astroid = {
      enable = true;
    };
    programs.mbsync.enable = true;
    programs.afew.enable = true;
    programs.notmuch.enable = true;
    programs.msmtp.enable = true;
    home.packages = with pkgs; [ mu ];
  };
}
