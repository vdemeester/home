{ config, pkgs, ... }:

{
  hardware = {
    pulseaudio = {
      enable = true;
      support32Bit = true;
      zeroconf = {
        discovery.enable = true;
        publish.enable = true;
      };
      tcp = {
        enable = true;
        anonymousClients = {
          allowAll = true;
          allowedIpRanges = [ "127.0.0.1" "192.168.12.0/24" "10.0.0.0/24" ];
        };
      };
      package = pkgs.pulseaudioFull;
    };
  };
  sound.mediaKeys.enable = true;
  
  # spotify & pulseaudio
  networking.firewall = {
    allowedTCPPorts = [ 57621 57622 4713 ];
    allowedUDPPorts = [ 57621 57622 ];
  };

  environment.systemPackages = with pkgs; [
    apulse       # allow alsa application to use pulse
    pavucontrol  # pulseaudio volume control
    pasystray    # systray application
    playerctl
  ];

  # We assume xserver runs when pulseaudio does
  services.xserver.displayManager.sessionCommands = "${pkgs.pasystray}/bin/pasystray &";
}
