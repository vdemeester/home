_: {
  services.kanshi = {
    enable = true;
    settings = [
      # Make it configurable per host
      {
        profile.name = "aomi";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "disable";
          }
          {
            criteria = "DP-1";
            status = "enable";
            position = "0,0";
            mode = "3440x1440";
            scale = 1.0;
          }
        ];
      }
      {
        profile.name = "home-undocked";
        profile.outputs = [
          # Output eDP-1 'AU Optronics 0xD291 Unknown'
          {
            criteria = "eDP-1";
            status = "enable";
            position = "0,0";
            mode = "1920x1200";
            scale = 1.0;
          }
        ];
      }
      {
        profile.name = "home-docked";
        profile.outputs = [
          # Old: Output eDP-1 'AU Optronics 0xD291 Unknown'
          # Output eDP-1 'Unknown 0xD291 Unknown'
          # Output DP-5 'LG Electronics LG ULTRAWIDE 0x0005D10C' (focused)
          # { criteria = "LG Electronics LG ULTRAWIDE 0x0000D50C"; status = "enable"; position = "0,0"; mode = "3440x1440"; scale = 1.0; }
          {
            criteria = "DP-5";
            status = "enable";
            position = "0,0";
            mode = "3440x1440";
            scale = 1.0;
          }
          # Use it as a "shareable" screen when needed
          {
            criteria = "eDP-1";
            status = "enable";
            position = "1460,1440";
            mode = "1920x1200";
            scale = 1.0;
          }
        ];
      }
    ];
  };
}
