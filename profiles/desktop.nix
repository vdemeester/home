{ configs, pkgs, ...}:

{
  imports = [
    ./printing.nix
    ./scanning.nix
  ];

  boot.loader.efi.canTouchEfiVariables = true;
  hardware.opengl.extraPackages = [ pkgs.vaapiIntel ];

}
