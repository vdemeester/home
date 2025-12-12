{
  pkgs,
  lib,
  config,
  hostname,
  ...
}:
{
  accounts.email = {
    maildirBasePath = "desktop/mails";
    accounts = {
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
  programs.mu = {
    enable = true;
    # Use XDG_DATA_HOME instead of XDG_CACHE_HOME to persist the mu database
    # across reboots and prevent re-indexing
    home = "${config.xdg.dataHome}/mu";
  };
  home.packages = with pkgs; [ mblaze ];

  # Override the default mu init activation to prevent it from wiping the index
  # The default activation runs `mu init` if the addresses don't match, but this
  # can trigger even when the database is populated if `mu info store` fails.
  # This override adds a check to skip init if the database has indexed messages.
  home.activation.runMuInit = lib.mkForce (
    config.lib.dag.entryAfter [ "writeBoundary" ] ''
      MUHOME="${config.xdg.dataHome}/mu"

      # Check if the database exists and has indexed messages
      if [ -d "$MUHOME/xapian" ] && [ -f "$MUHOME/xapian/iamglass" ]; then
        # Check if there are indexed messages
        MESSAGE_COUNT=$(${pkgs.mu}/bin/mu info store 2>/dev/null | ${pkgs.gawk}/bin/awk '/messages in store/{print $4}' || echo "0")
        if [ "$MESSAGE_COUNT" -gt "0" ]; then
          $VERBOSE_ECHO "Mu database exists with $MESSAGE_COUNT messages, skipping mu init"
          exit 0
        fi
      fi

      # Only run mu init if database doesn't exist or is empty
      if [ ! -d "$MUHOME" ]; then
        $VERBOSE_ECHO "Initializing mu database at $MUHOME"
        ${pkgs.mu}/bin/mu init \
          --maildir=$HOME/${config.accounts.email.maildirBasePath} \
          --muhome="$MUHOME" \
          --my-address=vincent@demeester.fr \
          ${if hostname == "kyushu" then "--my-address=vdemeest@redhat.com" else ""} \
          $VERBOSE_ARG
      fi
    ''
  );
}
