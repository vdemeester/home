{ config, pkgs, ... }:

{
  hardware = {
    opengl = {
      driSupport32Bit = true;
    };
  };
}
