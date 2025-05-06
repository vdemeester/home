{ pkgs, hostname, ... }:
{
  accounts.email = {
    maildirBasePath = "desktop/mails";
    accounts =
      {
        "icloud" = {
          primary = true;
          address = "vincent@demeester.fr";
          userName = "vdemeester@icloud.com";
          realName = "Vincent Demeester";
          passwordCommand = "${pkgs.passage}/bin/passage show mails/icloud/vdemeester";
          imap.host = "imap.mail.me.com";
          smtp.host = "smtp.mail.me.com";
          smtp.port = 587;
          mbsync = {
            enable = true;
            create = "both";
            expunge = "both";
            extraConfig = {
              channel = {
                Sync = "All";
              };
              account = {
                Timeout = 120;
                PipelineDepth = 1;
              };
            };
          };
          mu.enable = true;
          msmtp = {
            enable = true;
            extraConfig = {
              tls_starttls = "on";
            };
          };
        };
      }
      // (
        if (hostname == "kyushu") then
          {
            # Work emails
            "redhat" = {
              # primary = true; # because it's work, but it's really just for notmuch
              address = "vdemeest@redhat.com";
              userName = "vdemeest@redhat.com";
              realName = "Vincent Demeester";
              passwordCommand = "${pkgs.passage}/bin/passage show mails/gmail/redhat";
              imap.host = "imap.gmail.com";
              smtp.host = "smtp.gmail.com";
              flavor = "gmail.com";
              mbsync = {
                enable = true;
                create = "both";
                expunge = "both";
                # Sync everything *but* "[Gmail] All Mail" to get the "organized" view.
                patterns = [
                  "*"
                  "!area/github"
                  "!memo-list"
                  "![Gmail]*"
                  "[Gmail]/Sent Mail"
                  "[Gmail]/Starred"
                  "[Gmail]/Trash"
                  "[Gmail]/Drafts"
                ];
                extraConfig = {
                  channel = {
                    Sync = "All";
                  };
                  account = {
                    Timeout = 120;
                    PipelineDepth = 1;
                  };
                };
              };
              mu.enable = true;
              # aerc.enable = true;
              msmtp = {
                enable = true;
                # extraConfig = {
                #   tls_starttls = "on";
                # };
              };
            };
          }
        else
          { }
      );
  };

  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.mu.enable = true;
}
