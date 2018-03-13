{ config, pkgs, ... }:

with import ../accounts.nix;

{
  services.mopidy = {
    enable = true;
    extensionPackages = with pkgs; [
      mopidy-spotify
      mopidy-moped
      mopidy-mopify
    ];
    configuration = ''
      [spotify]
      username = ${spotify.user}
      password = ${spotify.password}
      client_id = ${spotify.client_id}
      client_secret = ${spotify.client_secret}
      [audio]
      output = pulsesink server=127.0.0.1
    '';
  };
}
