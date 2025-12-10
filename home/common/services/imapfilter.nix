{ pkgs, ... }:
{
  # Enable imapfilter package
  home.packages = [ pkgs.imapfilter ];

  # Create a systemd user service for imapfilter
  systemd.user.services.imapfilter = {
    Unit = {
      Description = "imapfilter - IMAP mail filtering utility";
      After = [ "network-online.target" ];
      Wants = [ "network-online.target" ];
    };

    Service = {
      Type = "oneshot";
      # Use passage to get the password
      # Verbose mode enabled for testing new filters
      ExecStart = "${pkgs.bash}/bin/bash -c '${pkgs.imapfilter}/bin/imapfilter -v -c ${./imapfilter-config.lua} -p <(${pkgs.passage}/bin/passage show mails/icloud/vdemeester)'";
      # Standard mode (use after testing is complete)
      # ExecStart = "${pkgs.bash}/bin/bash -c '${pkgs.imapfilter}/bin/imapfilter -c ${./imapfilter-config.lua} -p <(${pkgs.passage}/bin/passage show mails/icloud/vdemeester)'";
    };
  };

  # Create a systemd timer to run every 15 minutes
  systemd.user.timers.imapfilter = {
    Unit = {
      Description = "imapfilter timer - runs every 15 minutes";
    };

    Timer = {
      OnBootSec = "5min";
      OnUnitActiveSec = "15min";
      Unit = "imapfilter.service";
    };

    Install = {
      WantedBy = [ "timers.target" ];
    };
  };
}
