{ configs, pkgs, ...}:

{
  services = {
    printing = {
      enable = true;
      drivers = [ pkgs.gutenprint ];
    };
  };
}
