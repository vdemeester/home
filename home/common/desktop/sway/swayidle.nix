{ pkgs, ... }:
{
  home.packages = with pkgs; [
    swaylock
    swayidle
  ];
  services.swayidle = {
    enable = true;
    events = [
      { event = "before-sleep"; command = "${pkgs.swaylock}/bin/swaylock --daemonize -i $HOME/desktop/pictures/lockscreen"; }
      { event = "lock"; command = "${pkgs.swaylock}/bin/swaylock --daemonize -i $HOME/desktop/pictures/lockscreen"; }
    ];
    timeouts = [
      {
        timeout = 600;
        command = ''${pkgs.sway}/bin/swaymsg "output * power off"'';
        resumeCommand = ''${pkgs.sway}/bin/swaymsg "output * power on"'';
      }
    ];
  };
}
