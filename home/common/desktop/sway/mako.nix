_: {
  services.mako = {
    enable = true;
    font = "JetBrains Mono 12";
    defaultTimeout = 8000; # 5s timeout
    groupBy = "app-name,summary";
    # FIXME: hide pulseaudio notifications (maybe they don't show up without pasystray)
    settings = {
      width = 400;
      on-button-left = "dismiss";
      on-button-middle = "invoke-default-action";
      on-button-right = "dismiss";
      border-radius = 6;
      border-size = 3;
      border-color = "#db7508";
      format = "<b>%s</b>\n%b\n<i>%a</i>";
      icon-path = "/run/current-system/sw/share/icons/Qogir-dark:/run/current-system/sw/share/icons/hicolor";
    };
    criteria = {
      "urgency=low" = {
        background-color = "#282c30";
        text-color = "#888888";
        default-timeout = 2000;
      };
      "urgency=normal" = {
        background-color = "#282c30";
        text-color = "#ffffff";
        default-timeout = 5000;
      };
      "urgency=high" = {
        background-color = "#900000";
        text-color = "#ffffff";
        border-color = "#ff0000";
      };
      "category=\"build\"" = {
        anchor = "bottom-right";
        format = "<b>%s</b>\n%b";
      };
      "category=\"recording\"" = {
        anchor = "bottom-right";
        format = "<b>%s</b>\n%b";
      };
      "category=\"info\"" = {
        anchor = "center";
        format = "<b>%s</b> %b";
      };
      "mode=do-not-disturb" = {
        invisible = 1;
      };
      #[app-name="Google Chrome" body~="calendar.google.com.*"]
      #max-visible=2
    };
  };
}
