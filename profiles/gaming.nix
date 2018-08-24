{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [
      steam
      discord
    ];
  };
  hardware = {
    opengl = {
      driSupport32Bit = true;
    };
  };
}
