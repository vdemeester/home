_:
{
  services.mako = {
    enable = true;
    font = "JetBrains Mono 12";
    defaultTimeout = 8000; # 5s timeout
    groupBy = "app-name,summary";
    # FIXME: hide pulseaudio notifications (maybe they don't show up without pasystray)
    extraConfig = ''
      width=400
      on-button-left=dismiss
      on-button-middle=invoke-default-action
      on-button-right=dismiss
      border-radius=6
      border-size=3
      border-color=#db7508
      format=<b>%s</b>\n%b\n<i>%a</i>
      icon-path=/run/current-system/sw/share/icons/Qogir-dark:/run/current-system/sw/share/icons/hicolor

      [urgency=low]
      background-color=#282c30
      text-color=#888888
      default-timeout=2000

      [urgency=normal]
      background-color=#282c30
      text-color=#ffffff
      default-timeout=5000

      [urgency=high]
      background-color=#900000
      text-color=#ffffff
      border-color=#ff0000

      [app-name="pa-notify"]
      background-color=#0080ff
      text-color=#333333
      anchor=bottom-right
      format=<b>%s</b>\n%b

      [category="build"]
      anchor=bottom-right
      format=<b>%s</b>\n%b

      [category="recording"]
      anchor=bottom-right
      format=<b>%s</b>\n%b

      [category="info"]
      anchor=center
      format=<b>%s</b> %b

      #[app-name="Google Chrome" body~="calendar.google.com.*"]
      #max-visible=2

      [mode=do-not-disturb]
      invisible=1
    '';
  };
}
