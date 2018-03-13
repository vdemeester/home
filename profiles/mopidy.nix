{ config, pkgs, ... }:

with import ../accounts.nix;

{
  services.mopidy = {
    enable = true;
    extensionPackages = with pkgs; [
      mopidy-spotify
      mopidy-moped
      mopidy-mopify
      mopidy-youtube
    ];
    configuration = ''
    [core]
    restore_state = true
    [local]
    enabled = true
    [spotify]
    username = ${spotify.user}
    password = ${spotify.password}
    client_id = ${spotify.client_id}
    client_secret = ${spotify.client_secret}
    bitrate = 320
    timeout = 30
    [youtube]
    enabled = true
    [audio]
    mixer = software
    mixer_volume =
    output = pulsesink server=127.0.0.1
    buffer_time =
    '';
  };
}
