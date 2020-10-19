{ pkgs, ... }:

{
  home.packages = with pkgs; [ spotify spotify-tui ];
  services.spotifyd = {
    enable = true;
    package = (pkgs.spotifyd.override { withKeyring = true; withPulseAudio = true; withMpris = true; });
    settings = {
      global = {
        username = "vdemeester";
        use_keyring = "true";
        backend = "pulseaudio";
        device_name = "naruhodo"; # FIXME(vdemeester) change this
      };
    };
  };
}
