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
          passwordCommand = "${pkgs.gnupg}/bin/gpg -q --for-your-eyes-only --no-tty --exit-on-status-write-error --batch --passphrase-file ${config.home.homeDirectory}/sync/rh.pass -d ${config.home.homeDirectory}/desktop/documents/rh.pass.gpg";
          imap.host = "imap.gmail.com";
          smtp.host = "smtp.gmail.com";
          mbsync = {
            enable = true;
            create = "both";
            expunge = "both";
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
          passwordCommand = "${pkgs.gnupg}/bin/gpg -q --for-your-eyes-only --no-tty --exit-on-status-write-error --batch --passphrase-file ${config.home.homeDirectory}/sync/perso.pass -d ${config.home.homeDirectory}/desktop/documents/perso.pass.gpg";
          imap.host = "imap.gmail.com";
          smtp.host = "smtp.gmail.com";
          mbsync = {
            enable = true;
            create = "both";
            expunge = "both";
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
      preExec = "${config.xdg.configHome}/mbsync/preExec";
      postExec = "${config.xdg.configHome}/mbsync/postExec";
      frequency = "*:0/30";
    };
    xdg.configFile."mbsync/preExec" = {
      text = ''
      #!${pkgs.stdenv.shell}

      export NOTMUCH_CONFIG=${config.xdg.configHome}/notmuch/notmuchrc
      export NMBGIT=${config.xdg.dataHome}/notmuch/nmbug
      
      ${pkgs.coreutils}/bin/mkdir -p ${config.home.homeDirectory}/desktop/mails/redhat ${config.home.homeDirectory}/desktop/mails/perso
      ${pkgs.afew}/bin/afew -C  ${config.xdg.configHome}/notmuch/notmuchrc -m -v
      '';
      executable = true;
    };
    xdg.configFile."mbsync/postExec" = {
      text = ''
      #!${pkgs.stdenv.shell}

      export NOTMUCH_CONFIG=${config.xdg.configHome}/notmuch/notmuchrc
      export NMBGIT=${config.xdg.dataHome}/notmuch/nmbug
      
      ${pkgs.notmuch}/bin/notmuch new
      ${pkgs.afew}/bin/afew -C ${config.xdg.configHome}/notmuch/notmuchrc --tag --new -v
      ${pkgs.notmuch}/bin/notmuch tag -inbox tag:inbox
      '';
      executable = true;
    };
    programs.astroid = {
      enable = true;
      externalEditor = "emacsclient -c";
      extraConfig = {
        startup.queries.inbox = "tag:Inbox";
        startup.queries.inbox_perso = "tag:Inbox and tag:perso";
        startup.queries.inbox_redhat = "tag:Inbox and tag:redhat";
      };
    };
    programs.mbsync.enable = true;
    programs.afew = {
      enable = true;
      extraConfig = ''
        [SpamFilter]
        [KillThreadsFilter]
        [ListMailsFilter]
        [ArchiveSentMailsFilter]
        [FolderNameFilter]
        maildir_separator = /

        [MailMover]
        folders = perso/Inbox redhat/Inbox 
        rename = true

        perso/Inbox = 'NOT tag:Inbox':"perso/[Gmail]/All Mail"
        redhat/Inbox = 'NOT tag:Inbox':"redhat/[Gmail]/All Mail"
      '';
    };
    programs.notmuch.enable = true;
    programs.msmtp.enable = true;
    home.packages = with pkgs; [ mu ];
  };
}
